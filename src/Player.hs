module Player (
    zoomPlayer,
    updatePlayerTokens,
    giveDevelopment
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
    pTokens . at color . mapped .= amount'
  where
    notEnoughErr =
        "You do not have enough "
            <> show color
            <> " tokens to do that."

giveDevelopment :: DevelopmentId -> Update Player ()
giveDevelopment devId = pDevelopments %= (devId:)
