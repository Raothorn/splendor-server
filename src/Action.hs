module Action (
    execAction,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import DevelopmentLookup
import GameState
import Player
import Types
import Util

execAction :: Guid -> Action -> Update SplendorGame ()
----------------------------------
-- AcquireTokens Action
----------------------------------
execAction pg (AcquireTokens colors) = do
    amt <- case length colors of
        -- If only one color is chosen, take 2
        1 -> return 2
        -- If three colors are chosen, take one of each
        3 -> return 1
        _ -> liftErr "You must select 3 tokens of different colors, or 2 of the same color"

    -- For each color chosen, remove 'amt' from the pile and give it to the player
    forM_ colors $ \c -> do
        when (c == Gold) $
            liftErr "You cannot choose a gold token for this action"
        updateBankTokens (subtract amt) c
        zoomPlayer pg $ updatePlayerTokens (+ amt) c

    sgTurnNumber += 1

----------------------------------
-- PurchaseDevelopment Action
----------------------------------
execAction pg (PurchaseDevelopment deckIx devId goldAllocation) = do
    -- Allocate the gold jokers as the chosen gem type
    -- forM_ goldAllocation $ \(color, amt) -> do
    --     zoomPlayer pg $
    --         updatePlayerTokens (subtract amt) Gold >>
    --         updatePlayerTokens (+ amt) color

    let devData = getDevelopmentData devId

    forM_ allColors $ \color -> do
        -- The cost for this gem type
        let cost = getGemCost color devData

        -- Removes the tokens from the player pile. If they do not
        -- have enough, this will propogate an error
        zoomPlayer pg $ updatePlayerTokens (subtract cost) color

        -- Add the token back to the bank
        updateBankTokens (+ cost) color

    -- Removes the development from the shown pile and shows a new one if possible
    removeShownDevelopment deckIx devId

    -- Give the development to the player
    zoomPlayer pg $ pOwnedDevelopments %= (devId :)

    -- Increment the turn
    sgTurnNumber += 1

----------------------------------
-- ReserveDevelopment Action
----------------------------------
execAction pg (ReserveDevelopment deckIx devId) = do
    -- Removes the development from the shown pile and shows a new one if possible
    removeShownDevelopment deckIx devId

    -- Reserve the development to the player
    zoomPlayer pg $ pReservedDevelopments %= (devId :)

    -- Take a gold if possible
    gold <- useEither "" $ sgBank . at Gold
    let gold' = (clamp (0, maxBound) . subtract 1) gold
    sgBank . at Gold ?= gold'

    -- If a gold was taken, give it to the player
    when (gold' < gold) $
        zoomPlayer pg $
            updatePlayerTokens (+ 1) Gold

    -- Increment the turn
    sgTurnNumber += 1
execAction _ _ = return ()
