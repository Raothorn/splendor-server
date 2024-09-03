{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Types (
    SplendorGame,
    sgPlayers,
    sgBank,
    sgDecks,
    playerL,

    GemColor(..),
    allColors,

    newGame,
    Action (..),

    -- Aliases
    Update,
    TokenPiles,
    Guid,
    Player,

) where
import Data.Aeson 
import qualified Data.Map as M
import GHC.Generics
import Control.Monad.Trans.State.Lazy 
import Lens.Micro.TH
import Lens.Micro
import Lens.Micro.Platform()

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
    let playerMap = M.fromList $ map (, newPlayer) players
        bank = M.fromList $ map (,10) allColors
        decks = []
     in SplendorGame playerMap bank decks

newPlayer :: Player
newPlayer = M.fromList $ map (,0) allColors

----------------------------------
-- Gems
----------------------------------
data GemColor = Red | Blue
    deriving (Generic, Show, Ord, Eq)

allColors :: [GemColor]
allColors = [Red, Blue]

----------------------------------
-- Actions
----------------------------------
data Action
    = NoAction
    | AcquireTokensAction [GemColor]
    deriving (Generic, Show)

----------------------------------
-- Aeson Instances
----------------------------------
instance ToJSON GemColor
instance ToJSONKey GemColor
instance ToJSON SplendorGame

instance FromJSON GemColor
instance FromJSON Action

----------------------------------
-- Aliases
----------------------------------
type Update s a = StateT s (Either String) a

type Guid = String
type Player = TokenPiles

type TokenPiles = M.Map GemColor Int
type DevelopmentDeck = ()
----------------------------------
-- Lenses
----------------------------------
makeLenses ''SplendorGame

playerL :: Guid -> Traversal' SplendorGame Player
playerL guid = sgPlayers . at guid . traversed

