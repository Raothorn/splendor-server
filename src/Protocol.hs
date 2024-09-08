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

import State.GameState

import qualified Lenses.PlayerLenses as P
import qualified Lenses.GameLenses as G
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
    | Notification GameMessage
    | ErrorNotification String  
    | QueryResponse QueryResponse
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
-- instance ToJSON Query
-- instance ToJSON Action
-- instance ToJSON Request

instance ToJSON GemColor
instance ToJSONKey GemColor
instance ToJSON Player where
    toJSON player =
         object
                [ "tokens" .= (player ^. P.tokens)
                , "developmentGems" .= (player ^. P.allBonusGems)
                , "ownedDevelopments" .= (player ^. P.ownedDevelopments)
                , "reservedDevelopments" .= (player ^. P.reservedDevelopments)
                , "victoryPoints" .= (player ^. P.victoryPoints)
                , "username" .= (player ^. P.username)
                , "turnOrder" .= (player ^. P.turnOrder)
                ]

instance ToJSON SplendorGame where
    toJSON game =
        let currentPlayer = fmap (^. P.username) (getCurrentTurnPlayer game)
        in object
            [ "players" .= (game ^. G.players)
            , "tokenBank" .= (game ^. G.bank)
            , "decks" .= (game ^. G.decks)
            , "currentPlayer" .= currentPlayer
            , "gameOverSummary" .= (game ^. G.gameOver)
            ]

instance FromJSON GemColor
instance FromJSON Action
