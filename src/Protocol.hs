{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Protocol (
    Request (..),
    Response (..),
    Query (..),
    QueryResponse (..),
    encodeResponse,
) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.WebSockets (WebSocketsData (fromLazyByteString))
import Types 
import Lens.Micro
import DevelopmentLookup

import GameState
import Player 

----------------------------------
-- Types
----------------------------------

-- A "request" is the message the clients send to the server
data Request
    = JoinLobbyRequest String
    | ConnectRequest String
    | StartGameRequest
    | ReadyToPlayRequest
    | ActionRequest Action
    | QueryRequest Query
    | NoRequest
    deriving (Generic, Show)

-- A "response" is the message the server send to the client
data Response
    = LobbyUpdate [String]
    | GameUpdate SplendorGame
    | JoinLobbySuccess String
    | ErrorNotification String  | QueryResponse QueryResponse
    | NoResponse
    deriving (Generic, Show)

encodeResponse :: Response -> Text
encodeResponse = fromLazyByteString . encode

----------------------------------
-- Query
----------------------------------
data Query 
    = DevelopmentCostQ DevelopmentId
    | CanAffordQ Guid DevelopmentId
    | NoQ
    deriving (Generic, Show)

data QueryResponse
    = DevelopmentCostR [(GemColor, Int)]
    | CanAffordR Bool
    | NoR
    deriving (Generic, Show)

----------------------------------
-- Aeson Instances
----------------------------------
instance FromJSON Query
instance FromJSON Request

instance ToJSON QueryResponse
instance ToJSON Response

-- for testing
instance ToJSON Query
instance ToJSON Action
instance ToJSON Request 

instance ToJSON GemColor
instance ToJSONKey GemColor
instance ToJSON Player where
    toJSON player =
        let vps = sum $ map (developmentVp . getDevelopmentData) (player ^. pOwnedDevelopments)
         in object
                [ "tokens" .= (player ^. pTokens)
                , "developmentGems" .= getDevelopmentGems player
                , "ownedDevelopments" .= (player ^. pOwnedDevelopments)
                , "reservedDevelopments" .= (player ^. pReservedDevelopments)
                , "victoryPoints" .= vps
                , "username" .= (player ^. pUsername)
                , "turnOrder" .= (player ^. pTurnOrder)
                ]

instance ToJSON SplendorGame where
    toJSON game =
        let currentPlayer = fmap _pUsername (getCurrentTurnPlayer game)
        in object
            [ "players" .= (game ^. sgPlayers)
            , "tokenBank" .= (game ^. sgBank)
            , "decks" .= (game ^. sgDecks)
            , "currentPlayer" .= currentPlayer
            ]

instance FromJSON GemColor
instance FromJSON Action
