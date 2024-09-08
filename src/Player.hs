module Player (
    getDevelopmentGems,
    canAfford,
    zoomPlayer,
    updatePlayerTokens,
    hasReservedDevelopment,
    removeReservedDevelopment,
) where

import Control.Monad
import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.Mtl

import DevelopmentLookup
import Types
import Util

-----------------------------------------
-- Normal functions (for serialization)
-----------------------------------------
getDevelopmentGems :: Player -> TokenPiles
getDevelopmentGems player = M.fromList $ map (\c -> (c, getGemAmt c)) allColors
  where
    developments = map getDevelopmentData (player ^. pOwnedDevelopments)
    getGemAmt c = length (filter (\d -> developmentGem d == c) developments)

canAfford :: Player -> DevelopmentId -> Bool
canAfford player devId =
    let
        cost = developmentCost $ getDevelopmentData devId
        playerTokens = player ^. pTokens
        devGems = getDevelopmentGems player
        calcRemaining (color, amt) =
            max 0 $ amt - (playerTokens M.! color) - (devGems M.! color)
        remaining = map calcRemaining cost
    in
        sum remaining <= playerTokens M.! Gold

----------------------------------
-- Stateful functions
----------------------------------
zoomPlayer :: Guid -> Update Player () -> Update SplendorGame ()
zoomPlayer pg = zoom (sgPlayers . at pg . traversed)

updatePlayerTokens :: (Int -> Int) -> GemColor -> Update Player ()
updatePlayerTokens f color = do
    amount <- useEither "Cannot find tokens" (pTokens . at color)
    let amount' = f amount

    when (amount' < 0) $ liftErr notEnoughErr
    pTokens . at color . mapped .= amount'
  where
    notEnoughErr =
        "You do not have enough "
            <> show color
            <> " tokens to do that."

removeReservedDevelopment :: Int -> Update Player ()
removeReservedDevelopment devId = do
    developments <- use pReservedDevelopments
    if devId `elem` developments
        then pReservedDevelopments %= filter (/= devId)
        else liftErr "Development not reserved by player"

----------------------------------
-- Getters (don't zoom in for getters)
----------------------------------

hasReservedDevelopment :: Guid -> Int -> Update SplendorGame Bool
hasReservedDevelopment pg devId = do
    developments <- use $ playerL pg . pReservedDevelopments
    return $ devId `elem` developments
