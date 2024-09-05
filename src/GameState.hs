{-# LANGUAGE RankNTypes #-}
module GameState (
    updateBankTokens,
    removeShownDevelopment,
    getCurrentTurnPlayer
) where


import Control.Monad
import Control.Monad.Trans.Class
import Data.List
import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform()

import Types
import Util

----------------------------------
-- Normal functions
----------------------------------
getCurrentTurnPlayer :: SplendorGame -> Maybe Player
getCurrentTurnPlayer game = 
    let players = game ^. sgPlayers
        turn = game ^. sgPhase `mod` length players
        player =  M.toList (M.filter (\p -> p ^. pTurnOrder == turn) players)
        res = player ^? _head . _2
    in res

----------------------------------
-- Stateful functions
----------------------------------
updateBankTokens :: (Int -> Int) -> GemColor -> Update SplendorGame ()
updateBankTokens f color = do
    prevAmt <- useEither "Cannot find tokens" (sgBank . at color)
    if f prevAmt >= 0
        then sgBank . at color . mapped .= f prevAmt
        else lift $ Left "Not enough tokens in the pile to do that."


-- If the chosen development exists and is shown, remove it.
-- Then try to replace it with another development from the unshown decks
removeShownDevelopment :: Int -> DevelopmentId -> Update SplendorGame ()
removeShownDevelopment deckIx devId = do
    zoom (sgDecks . ix deckIx) $ do
        shown <- use shownDevs
        let (chosenDev, remaining) =  partition (== devId) shown

        case chosenDev of
            [] -> lift $ Left "The chosen development doesn't exist"
            x:_ -> do
                -- Update the shown cards
                shownDevs .= remaining

        unshown <- use unshownDevs
        -- Try to replace the removed card 
        case unshown of 
            -- If no cards available, exit without erroring.
            [] -> return ()
            -- Otherwise, draw a card from the unshown pile
            u:us -> do
                -- Add the newly drawn card to the end of the shown developments
                shownDevs %= (++ [u])
                unshownDevs .= us
