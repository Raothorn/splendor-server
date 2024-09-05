{-# LANGUAGE TupleSections #-}

module InitState (
    newGame,
) where

import qualified Data.Map as M

import Data.Foldable (toList)
import Data.Sequence (fromList, mapWithIndex)
import Types

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
    in
        SplendorGame
            { _sgPlayers = playerMap
            , _sgBank = bank
            , _sgDecks = decks
            , _sgTurnNumber = 0
            }

newPlayer :: String -> Int -> Player
newPlayer username order =
    let
        tokens = M.fromList $ map (,0) allColorsAndGold
        developments = []
    in
        Player
            { _pDevelopments = developments
            , _pTokens = tokens
            , _pUsername = username
            , _pTurnOrder = order
            }

draw :: Int -> DevelopmentDeck -> DevelopmentDeck
draw n (unshown, shown) =
    let (newShown, unshown') = splitAt n unshown
    in  (unshown', shown ++ newShown)
