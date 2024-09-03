{-# LANGUAGE RankNTypes #-}
module GameState (
    updateBankTokens
) where

import Control.Monad
import Control.Monad.Trans.Class

import Lens.Micro
import Lens.Micro.Mtl

import Types
import Util

updateBankTokens :: (Int -> Int) -> GemColor -> Update SplendorGame ()
updateBankTokens f color = do
    prevAmt <- useEither "Cannot find tokens" (sgBank . at color)
    if f prevAmt < 0
        then sgBank . at color . mapped .= f prevAmt
        else lift $ Left "Not enough tokens in the pile to do that."
