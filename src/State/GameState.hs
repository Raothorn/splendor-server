{-# LANGUAGE RankNTypes #-}

module State.GameState (
    getPlayer,
    getBankTokens,
    updateBankTokens,
    advanceTurn,
    removeShownDevelopment,
    getCurrentTurnPlayer,
    logEvent,
    notifyEvent,
) where

import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform ()


import qualified Lenses.GameLenses as G
import qualified Lenses.PlayerLenses as P
import qualified Lenses.DevelopmentLenses as D

import Util
import Types

-----------------------------------------
-- Normal functions (for serialization)
-----------------------------------------
getCurrentTurnPlayer :: SplendorGame -> Maybe Player
getCurrentTurnPlayer game =
    let players = game ^. G.players
        turn = game ^. G.turnNumber `mod` length players
        player = mapVals (M.filter (\p -> p ^. P.turnOrder == turn) players)
        res = player ^? _head
    in  res

----------------------------------
-- Stateful functions
----------------------------------
getPlayer :: Guid -> Update SplendorGame Player
getPlayer pg = useEither "Cannot find player" $ G.players . at pg

getBankTokens :: GemColor -> Update SplendorGame Int
getBankTokens color = use $ G.bank . at color . to (fromMaybe 0)

advanceTurn :: Update SplendorGame ()
advanceTurn = do
    turnNumber <- G.turnNumber <%= (+ 1)

    lastRound <- use G.lastRound
    numPlayers <- use $ G.players . to M.size
    let roundOver = turnNumber `mod` numPlayers == 0

    when (lastRound && roundOver) $ do
        winner <- getWinner
        G.gameOver ?= winner ^. P.username


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
    let deckIx = lookupDeck devId
    zoom (G.decks . ix deckIx) $ do
        shown <- use D.shownDevs
        let (chosenDev, remaining) = L.partition (== devId) shown

        case chosenDev of
            [] -> liftErr "The chosen development doesn't exist"
            _ -> do
                -- Update the shown cards
                D.shownDevs .= remaining

        unshown <- use D.unshownDevs
        -- Try to replace the removed card
        case unshown of
            -- If no cards available, exit without erroring.
            [] -> return ()
            -- Otherwise, draw a card from the unshown pile
            u : us -> do
                -- Add the newly drawn card to the end of the shown developments
                D.shownDevs %= (++ [u])
                D.unshownDevs .= us

getWinner :: Update SplendorGame Player
getWinner = do
    players <- use G.players
    return $ maximumByL P.victoryPoints players


logEvent :: LogEvent -> Update SplendorGame ()
logEvent event = G.messageLog %= (++ [event])

notifyEvent :: LogEvent -> Update SplendorGame ()
notifyEvent event = G.notificationQueue %= (++ [event])



















