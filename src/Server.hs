{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server (
    runServer,
) where

import Prelude hiding (log)

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State.Lazy

import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T

import Lens.Micro
import Lens.Micro.TH (makeLenses)

import qualified Network.WebSockets as WS

import ExecAction
import Protocol
import Types

import qualified Lenses.PlayerLenses as P
import qualified Lenses.GameLenses as G
import qualified Lenses.DevelopmentLenses as D
import State.GameState (getCurrentTurnPlayer)
----------------------------------
-- Types
----------------------------------
-- (Unique client GUID, WebSocket connection)
data Client = Client
    { _clientGuid :: Guid
    , _clientConn :: WS.Connection
    , -- The username is set only if the client has joined the lobby
      _clientUsername :: Maybe String
    }

data ServerState = ServerState
    { _lobbyClients :: [Client]
    , _appState :: Maybe SplendorGame
    }

makeLenses ''Client
makeLenses ''ServerState

----------------------------------
-- Client
----------------------------------
isJoined :: Client -> Bool
isJoined = isJust . _clientUsername

----------------------------------
-- ServerState
----------------------------------

newServerState :: ServerState
newServerState = ServerState [] Nothing

addClient :: Client -> ServerState -> ServerState
addClient client = over lobbyClients (client :)

removeClient :: Guid -> ServerState -> ServerState
removeClient guid = over lobbyClients (filter (\c -> c ^. clientGuid /= guid))

updateClient :: Client -> ServerState -> ServerState
updateClient client' = over lobbyClients (map update)
  where
    update c = if c ^. clientGuid == client' ^. clientGuid then client' else c

sendMessage :: Response -> Client -> IO ()
sendMessage msg client = WS.sendTextData (client ^. clientConn) (encodeResponse msg)

broadcastMessage :: Response -> ServerState -> IO ()
broadcastMessage msg server = forM_ (server ^. lobbyClients) $ sendMessage msg

lobbyUpdate :: ServerState -> Response
lobbyUpdate server =
    let lobbyPlayers = server ^. lobbyClients ^.. traversed . clientUsername
    in  LobbyUpdate (catMaybes lobbyPlayers)

handleLobbyMessage :: Request -> Client -> ServerState -> IO ServerState
handleLobbyMessage (JoinLobbyRequest username) client server = do
    let usernames = catMaybes $ server ^. lobbyClients ^.. traversed . clientUsername

    if username `elem` usernames
        then return server
        else do
            let
                client' = client & clientUsername ?~ username
                server' = updateClient client' server

            -- Send a success acknowledgement back to the client
            sendMessage (JoinLobbySuccess username) client'

            -- Broadcast the new lobby state to the other clients
            broadcastMessage (lobbyUpdate server') server'
            return server'
handleLobbyMessage StartGameRequest _ server = do
    let
        joinedClients = filter isJoined (server ^. lobbyClients)
        guidUsernames = map (\c -> (c ^. clientGuid, fromJust $ c ^. clientUsername)) joinedClients
        initState = newGame guidUsernames
        server' = server & appState ?~ initState

    broadcastMessage (GameUpdate initState) server'
    return server'

-- This must be sent by each client after they recieve the first game update
handleLobbyMessage ReadyToPlayRequest _ server = return server
handleLobbyMessage _ _ server = do
    putStrLn "Cannot parse request"
    return server

-- Any function that waits for input has to contain the server in an MVar
talkLobby :: BS.ByteString -> Client -> ServerState -> IO ServerState
talkLobby msg client server = do
    let decodedMessage = decode msg :: Maybe Request

    case decodedMessage of
        Just request -> handleLobbyMessage request client server
        Nothing -> return server

respondQuery :: Query -> SplendorGame -> IO Response

respondQuery (DevelopmentCostQ devId) _ = do
    let cost = devId ^. lookupDev . D.cost
    return $ QueryResponse (DevelopmentCostR cost)

respondQuery (CanAffordQ pg devId) gs = do
    let player = gs ^. G.players . at pg 

    case player of
        Just p -> return $ QueryResponse (CanAffordR (P.canAfford devId p))
        Nothing -> return $ QueryResponse NoR
    
respondQuery _ _ = return $ QueryResponse NoR

notify :: LogEvent -> NotificationType -> ServerState -> IO ()
notify ev notifType = broadcastMessage (Notification ev notifType)

talkGame :: BS.ByteString -> Client -> SplendorGame -> ServerState -> IO ServerState
talkGame msg client gs server = do
    let decodedMessage = decode msg :: Maybe Request

    case decodedMessage of
        Just (ActionRequest action) -> do
            putStrLn $ "Received action " <> show action
            let actionResult = execStateT (execAction (client ^. clientGuid) action) gs

            case actionResult of
                Left err -> do
                    putStrLn $ "Error: " <> err
                    notify (LogError err) NotifyError server
                    return server
                Right  gs' -> do
                    putStrLn "----OLD-----"
                    print gs
                    putStrLn "----NEW-----"
                    print gs'

                    forM_ (gs' ^. G.notificationQueue) $ \notif -> do 
                        notify notif NotifyInfo server

                    let gs'' = gs' & G.notificationQueue .~ []

                    broadcastMessage (GameUpdate gs'') server
                    return (server & appState ?~ gs'')

        Just (QueryRequest query) -> do
            putStrLn $ "Recieved query: " <> show query
            response <- respondQuery query gs
            sendMessage response client
            return server

        _ -> do
            putStrLn $ "Cannot parse client request " <> show msg
            return server


talk :: MVar ServerState -> Client -> IO ()
talk server client = 
    forever $ do
        -- Read the server state
        readMVar server >>= \s -> do
            -- Send an appropriate update depending on the app state
            case s ^. appState of
                Just gs -> sendMessage (GameUpdate gs) client
                Nothing -> sendMessage (lobbyUpdate s) client

        -- Wait for a message
        msg <- WS.receiveData (client ^. clientConn)

        -- Read the server state
        modifyMVar_ server $ \s -> do
            -- use an appropriate dispatcher depending on the app state
            case s ^. appState of
                Just gs -> talkGame msg client gs s
                Nothing -> talkLobby msg client s

attachClient :: Client -> MVar ServerState -> IO ()
attachClient client server = flip finally disconnect $ do
    modifyMVar_ server $ \s -> do
        let s' = addClient client s
        return s'
    putStrLn "Client attached"
    talk server client
  where
    disconnect = do
        modifyMVar_ server $ \s ->
            let s' = removeClient (client ^. clientGuid) s
            in  return s'
        putStrLn $
            "Client" <> show (client ^. clientGuid) <> " disconnected"

application :: MVar ServerState -> WS.ServerApp
application server pending = do
    conn <- WS.acceptRequest pending

    putStrLn "Pending connection..."
    WS.withPingThread conn 30 (return ()) $ do
        msg <- WS.receiveData conn
        let decodedMsg = decode msg :: Maybe Request

        case decodedMsg of
            Just (ConnectRequest guid) -> do
                readMVar server >>= \s -> do
                    if any (\c -> c ^. clientGuid == guid) (s ^. lobbyClients)
                        then putStrLn "GUID taken"
                        else do
                            let client = Client guid conn Nothing
                            attachClient client server
            _ ->
                T.putStrLn $
                    "Cannot decode connection request: "
                        <> WS.fromLazyByteString msg

runServer :: IO ()
runServer = do
    putStrLn "Listening on 9001..."
    server <- newMVar newServerState
    WS.runServer "0.0.0.0" 9001 $ application server
