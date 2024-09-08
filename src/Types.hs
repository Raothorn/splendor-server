{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Types (
    -- SplendorGame
    SplendorGame,
    mkSplendorGame,
    -- Player
    Player,
    mkPlayer,
    -- Action
    Action (..),
    -- Aliases
    Update,
    TokenPiles,
    Guid,
    DevelopmentId,
    GameMessage,
) where

import GHC.Generics

import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M

import Lens.Micro.Platform ()
import Lens.Micro.TH

import Types.Development
import Types.GemColor

----------------------------------
-- GameState
----------------------------------
data SplendorGame = SplendorGame
    { _players :: M.Map Guid Player
    , _bank :: TokenPiles
    , _decks :: [DevelopmentDeck]
    , _turnNumber :: Int
    , _lastRound :: Bool
    , _gameOver :: Maybe GameOverSummary
    }
    deriving (Generic, Show)

mkSplendorGame ::
    M.Map Guid Player ->
    TokenPiles ->
    [DevelopmentDeck] ->
    Int ->
    Bool ->
    Maybe GameOverSummary ->
    SplendorGame
mkSplendorGame = SplendorGame


----------------------------------
-- Player
----------------------------------
data Player = Player
    { _ownedDevelopments :: [DevelopmentId]
    , _reservedDevelopments :: [DevelopmentId]
    , _tokens :: TokenPiles
    , _username :: String
    , _turnOrder :: Int
    }
    deriving (Generic, Show)

mkPlayer ::
    [DevelopmentId] ->
    [DevelopmentId] ->
    TokenPiles ->
    String ->
    Int ->
    Player
mkPlayer = Player

----------------------------------
-- TurnPhase
----------------------------------

----------------------------------
-- GameOverSummary
----------------------------------
type GameOverSummary = String

----------------------------------
-- Actions
----------------------------------
data Action
    = NoAction
    | AcquireTokens [GemColor]
    | PurchaseDevelopment DevelopmentId [(GemColor, Int)]
    | ReserveDevelopment DevelopmentId
    deriving (Generic, Show)

----------------------------------
-- Aliases
----------------------------------
type Update s a = StateT s (Either String) a

type Guid = String

type TokenPiles = M.Map GemColor Int

type GameMessage = String

-- For now, the game phase only consists of the turn
-- type GamePhase = Int

----------------------------------
-- Lenses
----------------------------------
makeLenses ''SplendorGame

