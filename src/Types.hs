{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Types (
    -- SplendorGame
    SplendorGame,
    sgPlayers,
    sgBank,
    sgDecks,
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
    Player(..),
    pTokens,

    -- Misc/Refile
    newGame,

    -- Action
    Action (..),
    -- Aliases
    Update,
    TokenPiles,
    Guid,
    DevelopmentId,
) where

import Control.Monad.Trans.State.Lazy
import Data.Aeson
import qualified Data.Map as M
import GHC.Generics
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
    }
    deriving (Generic, Show)

-- TODO refactor elsewhere
newGame :: [Guid] -> SplendorGame
newGame players =
    let playerMap = M.fromList $ map (,newPlayer) players
        bank = M.fromList $ map (,10) allColors
        decks = []
     in SplendorGame playerMap bank decks

newPlayer :: Player
newPlayer =
    let tokens = M.fromList $ map (,0) allColors
        developments = []
     in Player developments tokens

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
-- Aeson Instances
----------------------------------
instance ToJSON GemColor
instance ToJSONKey GemColor
instance ToJSON Player
instance ToJSON SplendorGame

instance FromJSON GemColor
instance FromJSON Action

----------------------------------
-- Aliases
----------------------------------
type Update s a = StateT s (Either String) a

type Guid = String

type TokenPiles = M.Map GemColor Int
type DevelopmentId = Int

----------------------------------
-- Lenses
----------------------------------
makeLenses ''SplendorGame
makeLenses ''Player

playerL :: Guid -> Traversal' SplendorGame Player
playerL guid = sgPlayers . at guid . traversed

unshownDevs :: Lens' DevelopmentDeck [DevelopmentId]
unshownDevs = _2

shownDevs :: Lens' DevelopmentDeck [DevelopmentId]
shownDevs = _2
