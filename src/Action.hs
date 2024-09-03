module Action (
    execAction

) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Lens.Micro
import Lens.Micro.Mtl

import Types

execAction :: Guid -> Action -> Update ()

execAction pg (Take n)
    | n >= 1 && n <= 3 = do
        sgMarbles %= subtract n
        playerL pg %= (+ n)

    | otherwise = lift $ Left "You may only take from 1 to three marbles"

execAction _ _ = return ()

