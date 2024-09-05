{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Types (
    -- SplendorGame
    SplendorGame(..),
    sgPlayers,
    sgBank,
    sgDecks,
    sgPhase,
    playerL,
    -- Gems
    GemColor (..),
    allColors,
    -- Developments
    Development (..),
    DevelopmentDeck,
    shownDevs,
    unshownDevs,
    -- Player
    Player (..),
    pTokens,
    pDevelopments,
    pUsername,
    pTurnOrder,
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
    { _sgPlayers :: M.Map Guid Player
    , _sgBank :: TokenPiles
    , _sgDecks :: [DevelopmentDeck]
    , _sgPhase :: GamePhase
    }
    deriving (Generic, Show)

----------------------------------
-- Gems
----------------------------------
data GemColor = White | Blue | Green | Red | Black
    deriving (Generic, Show, Ord, Eq)

allColors :: [GemColor]
allColors = [White, Blue, Green, Red, Black]

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
    { _pDevelopments :: [DevelopmentId]
    , _pTokens :: TokenPiles
    , _pGold :: Int
    , _pUsername :: String
    , _pTurnOrder :: Int
   }
    deriving (Generic, Show)

----------------------------------
-- Actions
----------------------------------
data Action
    = NoAction
    | AcquireTokens [GemColor]
    | PurchaseDevelopment Int DevelopmentId
    deriving (Generic, Show)

----------------------------------
-- Aliases
----------------------------------
type Update s a = StateT s (Either String) a

type Guid = String

type TokenPiles = M.Map GemColor Int
type DevelopmentId = Int

-- For now, the game phase only consists of the turn
type GamePhase = Int

----------------------------------
-- Lenses
----------------------------------
makeLenses ''SplendorGame
makeLenses ''Player

playerL :: Guid -> Traversal' SplendorGame Player
playerL guid = sgPlayers . at guid . traversed

unshownDevs :: Lens' DevelopmentDeck [DevelopmentId]
unshownDevs = _1

shownDevs :: Lens' DevelopmentDeck [DevelopmentId]
shownDevs = _2

