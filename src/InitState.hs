{-# LANGUAGE TupleSections #-}

module InitState (
    newGame,
) where

import Data.Bifunctor
import qualified Data.Map as M

import Types
import Data.Foldable (toList)
import Data.Sequence (mapWithIndex, fromList)

newGame :: [(Guid, String)] -> SplendorGame
newGame players =
    let
        playerList = mapWithIndex (\ix (g,u) -> (g, newPlayer u ix)) (fromList players)
        playerMap = M.fromList (toList playerList)
        bank = M.fromList (map (,10) allColors)
        deck0 = draw 5 ([1..40],[])
        deck1 = draw 5 ([41..70],[])
        deck2 = draw 5 ([71..90],[])
        decks = [deck0, deck1, deck2]
    in
        SplendorGame playerMap bank decks 0

newPlayer :: String -> Int -> Player
newPlayer username order =
    let
        tokens = M.fromList $ map (,0) allColors
        developments = []
    in
        Player developments tokens username order

initDevelopmentDeck :: Int -> DevelopmentDeck
initDevelopmentDeck n = draw 5 ([1..n], [])

draw :: Int -> DevelopmentDeck -> DevelopmentDeck
draw n (unshown, shown) = 
    let (newShown, unshown') = splitAt 5 unshown
    in (unshown', shown ++ newShown)


