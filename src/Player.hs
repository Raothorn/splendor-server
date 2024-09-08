module Player (
    getDevelopmentGems,
    canAfford,
    getVictoryPoints,
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
import qualified Lenses.DevelopmentLenses as D
import qualified Lenses.GameLenses as G
import qualified Lenses.PlayerLenses as P
import Types
import Types.Development
import Types.GemColor
import Util

----------------------------------
-- Normal functions
----------------------------------
getDevelopmentGems :: Player -> TokenPiles
getDevelopmentGems player = M.fromList $ map (\c -> (c, getGemAmt c)) allColors
  where
    developments = player ^. P.ownedDevelopments ^.. each . lookupDev
    getGemAmt c = length (filter (\d -> d ^. D.gem == c) developments)

canAfford :: Player -> DevelopmentId -> Bool
canAfford player devId =
    let cost = devId ^. lookupDev . D.cost
        playerTokens = player ^. P.tokens
        devGems = getDevelopmentGems player
        calcRemaining (color, amt) =
            max 0 $ amt - (playerTokens M.! color) - (devGems M.! color)
        remaining = map calcRemaining cost
    in  sum remaining <= playerTokens M.! Gold

getVictoryPoints :: Player -> Int
getVictoryPoints player = sum devVps
  where
    devVps = player ^. P.ownedDevelopments ^.. each . lookupDev ^.. each . D.pp

hasReservedDevelopment :: Int -> Player -> Bool
hasReservedDevelopment devId player = devId `elem` player ^. P.reservedDevelopments

----------------------------------
-- Stateful functions
----------------------------------
zoomPlayer :: Guid -> Update Player () -> Update SplendorGame ()
zoomPlayer pg =
    zoom (G.players . at pg . traversed)

updatePlayerTokens :: (Int -> Int) -> GemColor -> Update Player ()
updatePlayerTokens f color = do
    amount <- useEither "Cannot find tokens" (P.tokens . at color)
    let amount' = f amount

    when (amount' < 0) $ liftErr notEnoughErr
    P.tokens . at color . mapped .= amount'
  where
    notEnoughErr =
        "You do not have enough "
            <> show color
            <> " tokens to do that."

removeReservedDevelopment :: Int -> Update Player ()
removeReservedDevelopment devId = do
    developments <- use P.reservedDevelopments
    if devId `elem` developments
        then P.reservedDevelopments %= filter (/= devId)
        else liftErr "Development not reserved by player"
