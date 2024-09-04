{-# LANGUAGE DeriveGeneric #-}

module Protocol (
    Request (..),
    Response (..),
    encodeResponse,
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.WebSockets (WebSocketsData (fromLazyByteString))
import Types 

----------------------------------
-- Types
----------------------------------

-- A "request" is the message the clients send to the server
data Request
    = JoinLobbyRequest String
    | ConnectRequest String
    | StartGameRequest
    | ReadyToPlayRequest
    | NoRequest
    deriving (Generic, Show)

-- A "response" is the message the server send to the client
data Response
    = LobbyUpdate [String]
    | GameUpdate SplendorGame
    | JoinLobbySuccess String
    | ErrorNotification String
    | NoResponse
    deriving (Generic, Show)

encodeResponse :: Response -> Text
encodeResponse = fromLazyByteString . encode

----------------------------------
-- Aeson Instances
----------------------------------
instance FromJSON Request

instance ToJSON Response
