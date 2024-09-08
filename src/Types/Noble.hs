module Types.Noble (
    Noble,
    NobleId,
    lookupNoble
) where

import Lens.Micro 

import Types.GemColor

type Noble = [(GemColor, Int)]

type NobleId = Int

lookupNoble :: NobleId -> Noble
lookupNoble 0 = [(Black, 1), (Red, 1)]
lookupNoble _ = []
