{-# LANGUAGE RankNTypes #-}
module Util (
    useEither,
    addClamp,
    maybeToEither,
    liftErr,
    mapVals
) where
import Control.Monad
import Control.Monad.Trans.Class

import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.Mtl

import Types

-- TODO make more general in case we zoom the state
useEither :: String -> Lens' s (Maybe b) -> Update s b
useEither err l = do
    value <- use l
    case value of
        Just v -> return v
        Nothing -> liftErr err

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err = maybe (Left err) Right

addClamp :: (Int, Int) -> Int -> Int -> Int
addClamp range a b = clamp range (a + b)

clamp :: (Int, Int) -> Int -> Int
clamp (minVal, maxVal) n
    | minVal > maxVal = n
    | otherwise =
        if n < minVal
            then minVal
            else min n maxVal

liftErr :: String -> Update s a
liftErr =  lift . Left

mapVals :: M.Map k v -> [v]
mapVals = map snd . M.toList
