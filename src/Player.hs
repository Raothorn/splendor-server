module Player (
    zoomPlayer,
    updatePlayerTokens,
) where

import Control.Monad.Trans.Class

import Lens.Micro
import Lens.Micro.Mtl

import Types
import Util

zoomPlayer :: Guid -> Update Player () -> Update SplendorGame ()
zoomPlayer pg = zoom (sgPlayers . at pg . traversed)

updatePlayerTokens :: (Int -> Int) -> GemColor -> Update Player ()
updatePlayerTokens f color = do
    prevAmt <- useEither "Cannot find tokens" (pTokens . at color)
    if f prevAmt >= 0
        then pTokens . at color . mapped .= f prevAmt
        else lift $ Left err
  where
    err = "The player does not have enough "
            <> show color
            <> " tokens to do that."

-- -- Getters (don't zoom in on the player for getters)
-- getPlayerTokens :: Guid -> Color -> Update SplendorGame Int
