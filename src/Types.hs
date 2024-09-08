{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Types (
    -- SplendorGame
    SplendorGame ,
    mkSplendorGame,
    -- Gems
    GemColor (..),
    allColors,
    allColorsAndGold,
    -- Developments
    Development (..),
    DevelopmentDeck,
    shownDevs,
    unshownDevs,
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
) where

import GHC.Generics

import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.Platform ()
import Lens.Micro.TH

----------------------------------
-- GameState
----------------------------------
data SplendorGame = SplendorGame
    { _players    :: M.Map Guid Player
    , _bank       :: TokenPiles
    , _decks      :: [DevelopmentDeck]
    , _turnNumber :: Int
    , _lastRound  :: Bool
    }
    deriving (Generic, Show)

mkSplendorGame :: M.Map Guid Player -> TokenPiles -> [DevelopmentDeck] -> Int -> Bool -> SplendorGame
mkSplendorGame = SplendorGame

----------------------------------
-- Gems
----------------------------------
data GemColor = White | Blue | Green | Red | Black | Gold
    deriving (Generic, Show, Ord, Eq)

allColors :: [GemColor]
allColors = [White, Blue, Green, Red, Black]

allColorsAndGold :: [GemColor]
allColorsAndGold = [White, Blue, Green, Red, Black, Gold]

----------------------------------
-- Developments
----------------------------------
data Development = Development
    { developmentGem :: GemColor
    , developmentCost :: [(GemColor, Int)]
    , developmentVp :: Int
    , developmentId :: DevelopmentId
    }
    deriving (Generic, Show)

-- The first value in the tuple is the unshown cards, the second is the shown
type DevelopmentDeck = ([DevelopmentId], [DevelopmentId])

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

mkPlayer :: [DevelopmentId] -> [DevelopmentId] -> TokenPiles -> String -> Int -> Player
mkPlayer = Player
----------------------------------
-- TurnPhase
----------------------------------

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
type DevelopmentId = Int

-- For now, the game phase only consists of the turn
-- type GamePhase = Int

----------------------------------
-- Lenses
----------------------------------
makeLenses ''SplendorGame

unshownDevs :: Lens' DevelopmentDeck [DevelopmentId]
unshownDevs = _1

shownDevs :: Lens' DevelopmentDeck [DevelopmentId]
shownDevs = _2
