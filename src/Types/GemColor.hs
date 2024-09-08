{-# LANGUAGE DeriveGeneric #-}
module Types.GemColor (
    GemColor(..),
    allColors,
    allColorsAndGold,
    TokenPiles,
) where

import GHC.Generics
import qualified Data.Map as M

data GemColor = White | Blue | Green | Red | Black | Gold
    deriving (Generic, Show, Ord, Eq)

allColors :: [GemColor]
allColors = [White, Blue, Green, Red, Black]

allColorsAndGold :: [GemColor]
allColorsAndGold = [White, Blue, Green, Red, Black, Gold]

type TokenPiles = M.Map GemColor Int
