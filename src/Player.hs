module Player (
    zoomPlayer,
    updatePlayerTokens,
) where

import Control.Monad
import Control.Monad.Trans.Class

import Lens.Micro
import Lens.Micro.Mtl

import Types
import Util

zoomPlayer :: Guid -> Update Player () -> Update SplendorGame ()
zoomPlayer pg = zoom (sgPlayers . at pg . traversed)

updatePlayerTokens :: (Int -> Int) -> GemColor -> Update Player ()
updatePlayerTokens f color = do
    amount <- useEither "Cannot find tokens" (pTokens . at color)
    let amount' = f amount

    when (amount' < 0) $ liftErr notEnoughErr
    pTokens . at color . mapped .= f amount'
  where
    notEnoughErr =
        "You do not have enough "
            <> show color
            <> " tokens to do that."

-- -- Getters (don't zoom in on the player for getters)
-- getPlayerTokens :: Guid -> Color -> Update SplendorGame Int
