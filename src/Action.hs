module Action (
    execAction,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl hiding ((<%=))

import DevelopmentLookup
import GameState
import Player
import Types
import Util 

{- FOURMOLU_DISABLE -}
execAction :: Guid -> Action -> Update SplendorGame ()

----------------------------------
-- AcquireTokens Action
----------------------------------
execAction pg (AcquireTokens colors) 
    -- If three colors are chosen, take one of each
    | length colors == 3 = acquireTokens pg 1 colors
    -- If only one color is chosen, take 2
    | length colors == 1 = acquireTokens pg 2 colors
    | otherwise = liftErr  "You must pick 3 tokens of different types or 2 of the same type."

----------------------------------
-- PurchaseDevelopment Action
----------------------------------
execAction pg (PurchaseDevelopment deckIx devId) = do
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
    gold <- use $ sgBank . at Gold
    let gold' = fmap (clamp (0, maxBound) . subtract 1) gold
    sgBank . at Gold .= gold'

    -- If a gold was taken, give it to the player
    when (gold' < gold) $
        zoomPlayer pg $ updatePlayerTokens (+ 1) Gold
    
    -- Increment the turn
    sgTurnNumber += 1

execAction _ _ = return ()
{- FOURMOLU_ENABLE -}

----------------------------------
-- Helpers
----------------------------------
acquireTokens :: Guid -> Int -> [GemColor] -> Update SplendorGame ()
acquireTokens pg amt colors = do
    forM_ colors $ \c -> do
        when (c == Gold) $
            liftErr "You cannot choose a gold token for this action"
        updateBankTokens (subtract amt) c
        zoomPlayer pg $ updatePlayerTokens (+ amt) c
    -- Increment the turn
    sgTurnNumber += 1
