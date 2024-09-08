module State.PlayerState (
    zoomPlayer,
    updatePlayerTokens,
    removeReservedDevelopment,
) where

import Control.Monad

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform()

import qualified Lenses.GameLenses as G
import qualified Lenses.PlayerLenses as P
import Types
import Util

----------------------------------
-- Normal functions
----------------------------------

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
