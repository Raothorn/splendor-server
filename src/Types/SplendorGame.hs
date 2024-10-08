{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.SplendorGame (
    SplendorGame,
    newGame
) where

import GHC.Generics

import qualified Data.Map as M
import Data.Foldable (toList)
import Data.Sequence (mapWithIndex, fromList)

import Types.Player
import Types.Development
import Types.GemColor
import Types.Alias
import Types.Noble
import Types.LogMessage (LogEvent)

----------------------------------
-- GameState
----------------------------------
data SplendorGame = SplendorGame
    { _players :: M.Map Guid Player
    , _bank :: TokenPiles
    , _decks :: [DevelopmentDeck]
    , _nobles :: [NobleId]
    , _turnNumber :: Int
    , _lastRound :: Bool
    , _gameOver :: Maybe GameOverSummary
    , _messageLog :: [LogEvent]
    , _notificationQueue :: [LogEvent]
    }
    deriving (Generic, Show)


newGame :: [(Guid, String)] -> SplendorGame
newGame players =
    let
        playerList = mapWithIndex (\ix (g, u) -> (g, newPlayer u ix)) (fromList players)
        playerMap = M.fromList (toList playerList)
        bank = M.fromList (map (,7) allColors ++ [(Gold, 5)])
        deck0 = draw 5 ([1 .. 40], [])
        deck1 = draw 5 ([41 .. 70], [])
        deck2 = draw 5 ([71 .. 90], [])
        decks = [deck0, deck1, deck2]
        nobles = [0, 1, 2]
    in
        SplendorGame playerMap bank decks nobles 0 False Nothing [] []

draw :: Int -> DevelopmentDeck -> DevelopmentDeck
draw n (unshown, shown) = 
    let (newShown, unshown') = splitAt n unshown
    in (unshown', shown ++ newShown)
