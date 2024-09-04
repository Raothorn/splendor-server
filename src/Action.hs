module Action (
    execAction,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

import DevelopmentLookup
import GameState
import Player
import Types
import Util (addClamp)

{- FOURMOLU_DISABLE -}
execAction :: Guid -> Action -> Update SplendorGame ()

----------------------------------
-- AcquireTokens Action
----------------------------------
execAction pg (AcquireTokens colors) 
    | length colors == 3 = do
        forM_ colors $ \c -> do
            updateBankTokens (subtract 1) c
            zoomPlayer pg $ updatePlayerTokens (+ 1) c

    | length colors == 1 = do
        let color = head colors
        updateBankTokens (subtract 2) color
        zoomPlayer pg $ updatePlayerTokens (+ 2) color
            
    | otherwise = lift $ Left "You must pick 3 tokens of different types or 2 of the same type."

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

        -- Removes the development from the shown pile and shows a new one if possible
        removeShownDevelopment deckIx devId

execAction _ _ = return ()
{- FOURMOLU_ENABLE -}
