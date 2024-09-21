{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Protocol (
    Request (..),
    Response (..),
    Query (..),
    QueryResponse (..),
    NotificationType(..),
    encodeResponse,
) where

import Data.Aeson
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
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
    | Notification LogEvent NotificationType
    | QueryResponse QueryResponse
    | NoResponse
    deriving (Generic, Show)

encodeResponse :: Response -> Text
encodeResponse = fromLazyByteString . encode

data NotificationType = NotifyError | NotifyInfo
    deriving (Show, Generic)

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

instance ToJSON NotificationType
instance ToJSON Action
instance ToJSON LogEvent where
    toJSON = A.String . T.pack . show

-- for testing
-- instance ToJSON Query
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
                , "nobles" .= (player ^. P.nobles)
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
            , "nobles" .= (game ^. G.nobles)
            , "currentPlayer" .= currentPlayer
            , "gameOverSummary" .= (game ^. G.gameOver)
            , "messageLog" .= (game ^. G.messageLog)
            ]

instance FromJSON GemColor
instance FromJSON Action
