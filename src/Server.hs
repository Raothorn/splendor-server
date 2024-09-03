{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server (
    runServer,
) where

import Control.Concurrent
import Control.Monad

import Control.Exception
import Control.Monad.Trans.State (execStateT)
import Data.Aeson
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Text.IO as T

import Lens.Micro
import Lens.Micro.TH (makeLenses)

import qualified Network.WebSockets as WS

import Action
import Protocol
import Types
import Control.Monad.Trans.State.Lazy

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
     in LobbyUpdate (catMaybes lobbyPlayers)

gameUpdate :: ServerState -> Maybe Response
gameUpdate server = fmap GameUpdate (server ^. appState)

handleLobbyMessage :: Request -> Client -> ServerState -> IO ServerState
handleLobbyMessage (JoinLobbyRequest username) client server = do
    let client' = client & clientUsername ?~ username
        server' = updateClient client' server

    -- Send a success acknowledgement back to the client
    sendMessage (JoinLobbySuccess username) client'

    -- Broadcast the new lobby state to the other clients
    broadcastMessage (lobbyUpdate server') server'
    return server'
handleLobbyMessage StartGameRequest _ server = do
    let joinedClients = filter isJoined (server ^. lobbyClients)
        initState = newGame (joinedClients ^.. traversed . clientGuid)
        server' = server & appState ?~ initState

    broadcastMessage (GameUpdate initState) server'
    return server'

-- This must be sent by each client after they recieve the first game update
handleLobbyMessage ReadyToPlayRequest _ server = return server
    
handleLobbyMessage _ _ server = do
    putStrLn "Cannot parse request"
    return server

-- Any function that waits for input has to contain the server in an MVar
talkLobby :: MVar ServerState -> Client -> IO ()
talkLobby server client = do
    -- Send the state of the lobby to the client
    readMVar server >>= \s -> sendMessage (lobbyUpdate s) client

    -- Wait for a message from the client
    msg <- WS.receiveData (client ^. clientConn)
    let decodedMessage = decode msg :: Maybe Request

    case decodedMessage of
        Just request ->
            modifyMVar_ server $ \s -> handleLobbyMessage request client s
        Nothing -> return ()

    talk server client

talkGame :: MVar ServerState -> Client -> SplendorGame -> IO ()
talkGame server client gs = do
    -- Send the state of the game to the client
    sendMessage (GameUpdate gs) client

    msg <- WS.receiveData (client ^. clientConn)
    let decodedMessage = decode msg :: Maybe Action

    case decodedMessage of
        Just action -> do
            putStrLn $ "Received action " <> show action
            let actionResult = execStateT (execAction (client ^. clientGuid) action) gs
            
            case actionResult of
                Left err -> putStrLn $ "Error: " <> err
                Right gs' -> do
                    modifyMVar_ server $ \s -> do
                        let s' = s & appState ?~ gs'
                        broadcastMessage (GameUpdate gs') s'
                        return s'

        Nothing -> do
            putStrLn $ "Cannot parse action " <> show msg

    talk server client

talk :: MVar ServerState -> Client -> IO ()
talk server client = do
    -- Read the server state
    readMVar server >>= \s -> do
        -- Dispatch to a dedicated handler depending on whether the game has started
        case s ^. appState of
            Just gs -> talkGame server client gs
            Nothing -> talkLobby server client

attachClient :: Client -> MVar ServerState -> IO ()
attachClient client server = flip finally disconnect $ do
    modifyMVar_ server $ \s -> do
        let s' = addClient client s
        return s'
    putStrLn "Client attached"
    talk server client
  where
    disconnect = do
        s <- modifyMVar_ server $ \s ->
            let s' = removeClient (client ^. clientGuid) s
             in return s'
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
    server <- newMVar newServerState
    WS.runServer "127.0.0.1" 9001 $ application server
