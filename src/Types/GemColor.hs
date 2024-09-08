{-# LANGUAGE DeriveGeneric #-}
module Types.GemColor (
    GemColor(..),
    allColors,
    allColorsAndGold,
) where

import GHC.Generics

data GemColor = White | Blue | Green | Red | Black | Gold
    deriving (Generic, Show, Ord, Eq)

allColors :: [GemColor]
allColors = [White, Blue, Green, Red, Black]

allColorsAndGold :: [GemColor]
allColorsAndGold = [White, Blue, Green, Red, Black, Gold]
