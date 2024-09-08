{-# LANGUAGE RankNTypes #-}
module Util (
    useEither,
    useEither',
    clamp,
    liftMaybe,
    liftErr,
    mapVals,
    maximumByL,
    toDefault
) where
import Control.Monad.State.Lazy

import Data.Maybe
import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.Mtl

useEither :: String -> Lens' s (Maybe b) -> StateT s (Either String) b
useEither err l = do
    value <- use l
    case value of
        Just v -> return v
        Nothing -> liftErr err

useEither' :: Lens' s (Maybe b) -> StateT s (Either String) b
useEither' = useEither "Error converting Maybe to Either"

liftErr :: String -> StateT s (Either String) a
liftErr =  lift . Left

liftMaybe :: Maybe a -> StateT s (Either String) a
liftMaybe x = do 
    let eitherX  = maybeToEither "Error lifting Nothing" x
    lift eitherX

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither err = maybe (Left err) Right

clamp :: (Int, Int) -> Int -> Int
clamp (minVal, maxVal) n
    | minVal > maxVal = n
    | otherwise =
        if n < minVal
            then minVal
            else min n maxVal


mapVals :: M.Map k v -> [v]
mapVals = map snd . M.toList

maximumByL :: (Foldable t, Ord n) => SimpleGetter a n -> t a -> a
maximumByL l = foldr1 f
    where
        f x mx = if x ^. l > mx ^. l then x else mx

toDefault :: a -> SimpleGetter (Maybe a) a
toDefault def = to $ fromMaybe def

