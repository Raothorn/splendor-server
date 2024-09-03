module Action (
    execAction

) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

import Types
import GameState
import Player
import Util (addClamp)

execAction :: Guid -> Action -> Update SplendorGame ()

execAction pg (AcquireTokensAction colors) 
    | length colors == 3 = do
        forM_ colors $ \c -> do
            updateBankTokens (subtract 1) c
            zoomPlayer pg $ updatePlayerTokens (+ 1) c

    | length colors == 1 = do
        let color = head colors
        updateBankTokens (subtract 2) color
        zoomPlayer pg $ updatePlayerTokens (+ 2) c
            
    | otherwise = lift $ Left "You must pick 3 tokens of different types or 2 of the same type."

execAction _ _ = return ()

