{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Types (
    SplendorGame,
    sgMarbles,
    sgPlayers,
    playerL,

    newGame,
    Action (..),

    Update,
    Guid,
    Player
) where
import Data.Aeson 
import qualified Data.Map as M
import GHC.Generics
import Control.Monad.Trans.State.Lazy 
import Lens.Micro.TH
import Lens.Micro
import Lens.Micro.Platform

----------------------------------
-- GameState
----------------------------------
data SplendorGame = SplendorGame
    { _sgMarbles :: Int
    , _sgPlayers :: M.Map Guid Player
    }
    deriving (Generic, Show)

-- TODO refactor elsewhere
newGame :: [Guid] -> SplendorGame
newGame players =
    let playerMap = M.fromList $ map (,0) players
     in SplendorGame 12 playerMap


----------------------------------
-- Actions
----------------------------------
data Action
    = NoAction
    | Take Int
    deriving (Generic, Show)

----------------------------------
-- Aeson Instances
----------------------------------
instance ToJSON SplendorGame

instance FromJSON Action

----------------------------------
-- Aliases
----------------------------------
type Update a = StateT SplendorGame (Either String) a

type Guid = String
type Player = Int

----------------------------------
-- Lenses
----------------------------------
makeLenses ''SplendorGame

playerL :: Guid -> Traversal' SplendorGame Player
playerL guid = sgPlayers . at guid . traversed
