{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ExecAction (
    execAction,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl

import qualified Data.List as L

import qualified Lenses.DevelopmentLenses as D
import qualified Lenses.GameLenses as G
import qualified Lenses.PlayerLenses as P

import State.GameState
import State.PlayerState

import qualified GameOptions as Op
import Types
import Util


execAction :: Guid -> Action -> Update SplendorGame ()
execAction pg action = do
    player <- getPlayer pg
    logEvent (PlayerChoseAction player action)
    execAction' pg action

execAction' :: Guid -> Action -> Update SplendorGame ()
----------------------------------
-- AcquireTokens Action
----------------------------------
execAction' pg (AcquireTokens colors) = do
    -- If 2 of one color token is taken, there must be 4 in the pile
    (requires4, amt) <- case length colors of
        -- If only one color is chosen, take 2
        1 -> return (True, 2)
        -- If three colors are chosen, take one of each
        3 -> return (False, 1)
        _ -> liftErr "You must select 3 tokens of different colors, or 2 of the same color"

    -- For each color chosen, remove 'amt' from the pile and give it to the player
    forM_ colors $ \c -> do
        when (c == Gold) $
            liftErr "You cannot choose a gold token for this action"

        tokens <- getBankTokens c
        when (requires4 && tokens < 4) $
            liftErr "You can't perform this action unless there are at least 4 tokens in the pile"

        updateBankTokens (subtract amt) c
        zoomPlayer pg $ updatePlayerTokens (+ amt) c

    advanceTurn

----------------------------------
-- PurchaseDevelopment Action
----------------------------------
execAction' pg (PurchaseDevelopment devId goldAllocation) = do
    let devData = devId ^. lookupDev

    player <- getPlayer pg
    forM_ allColors $ \color -> do
        -- The bonus the player gets from developments
        let bonus = player ^. P.bonusGems color

        -- The cost for this gem type after the bonus
        devCost <- liftMaybe (L.lookup color $ devData ^. D.cost)
        goldAllocation <- liftMaybe (L.lookup color goldAllocation)

        -- The actual amount of tokens the player needs to spend
        let playerCost = max 0 $ devCost - bonus - goldAllocation

        zoomPlayer pg $ do 
            -- Remove the tokens from the player pile
            updatePlayerTokens (subtract playerCost) color
            -- Take the gold from the player
            updatePlayerTokens (subtract goldAllocation) Gold

        -- Add the token back to the bank
        updateBankTokens (+ playerCost) color
        -- give the gold tokens back to the bank
        updateBankTokens (+ goldAllocation) Gold

    -- Determine if the development is coming from the board or the player's reserve
    let isReserve = devId `elem` player ^. P.reservedDevelopments
    if isReserve
        then -- Remove the card from the player's reserve
            zoomPlayer pg $ removeReservedDevelopment devId
        else -- Removes the development from the shown pile and shows a new one if possible
            removeShownDevelopment devId

    -- Give the development to the player
    zoomPlayer pg $ do
        P.ownedDevelopments %= (devId :)

    -- Check if the player qualifies for any of the nobles
    player <- getPlayer pg
    nobles <- use G.nobles
    forM_ nobles $ \noble -> do
        when (P.canVisitNoble noble player) $ do
            zoomPlayer pg $ P.nobles %= (noble :)
            G.nobles %= filter (/= noble)

    -- If the player now has 15 or more victory points, set the "last round" flag
    -- and notify.
    player <- getPlayer pg
    when (player ^. P.victoryPoints >= Op.vpsToWin) $ do
        G.lastRound .= True
        -- log and notify
        let event = LastRound player
        logEvent event
        notifyEvent event

    advanceTurn

----------------------------------
-- ReserveDevelopment Action
----------------------------------
execAction' pg (ReserveDevelopment devId) = do
    player <- getPlayer pg
    when (length (player ^. P.reservedDevelopments) >= 3) $
        liftErr "You may not reserve more than three developments"
    
    -- Removes the development from the shown pile and shows a new one if possible
    removeShownDevelopment devId

    -- Reserve the development to the player
    zoomPlayer pg $ P.reservedDevelopments %= (devId :)

    -- Take a gold if possible
    gold <- useEither "Cannot find gold in the bank" $ G.bank . at Gold
    let gold' = (clamp (0, maxBound) . subtract 1) gold
    G.bank . at Gold ?= gold'

    -- If a gold was taken, give it to the player
    when (gold' < gold) $
        zoomPlayer pg $
            updatePlayerTokens (+ 1) Gold

    advanceTurn

execAction' _ _ = return ()
