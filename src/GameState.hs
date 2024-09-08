{-# LANGUAGE RankNTypes #-}
module GameState (
    getPlayer,
    getBankTokens,
    updateBankTokens,
    removeShownDevelopment,
    getCurrentTurnPlayer
) where


import qualified Data.List as L
import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform()

import Types
import Util
import DevelopmentLookup (getDeckIndex)
import Data.Maybe

import qualified Lenses.PlayerLenses as P
import qualified Lenses.GameLenses as G

-----------------------------------------
-- Normal functions (for serialization)
-----------------------------------------
getCurrentTurnPlayer :: SplendorGame -> Maybe Player
getCurrentTurnPlayer game = 
    let players = game ^. G.players
        turn = game ^. G.turnNumber `mod` length players
        player =  mapVals (M.filter (\p -> p ^. P.turnOrder == turn) players)
        res = player ^? _head
    in res

----------------------------------
-- Stateful functions
----------------------------------
getPlayer :: Guid -> Update SplendorGame Player 
getPlayer pg = useEither "Cannot find player" $ G.players . at pg

getBankTokens :: GemColor -> Update SplendorGame Int
getBankTokens color = use $ G.bank . at color . to (fromMaybe 0)

updateBankTokens :: (Int -> Int) -> GemColor -> Update SplendorGame ()
updateBankTokens f color = do
    prevAmt <- useEither "Cannot find tokens" (G.bank . at color)
    if f prevAmt >= 0
        then G.bank . at color . mapped .= f prevAmt
        else liftErr "Not enough tokens in the pile to do that."


-- If the chosen development exists and is shown, remove it.
-- Then try to replace it with another development from the unshown decks
removeShownDevelopment :: DevelopmentId -> Update SplendorGame ()
removeShownDevelopment devId = do
    let deckIx = getDeckIndex devId
    zoom (G.decks . ix deckIx) $ do
        shown <- use shownDevs
        let (chosenDev, remaining) =  L.partition (== devId) shown

        case chosenDev of
            [] -> liftErr "The chosen development doesn't exist"
            _ -> do
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
