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
lookupNoble 0 = [(Red, 4), (Green, 4)]
lookupNoble 1 = [(Black, 3), (Blue, 3), (White, 3)]
lookupNoble 2 = [(Black, 3), (Red, 3), (White, 3)]
lookupNoble 3 = [(Black, 4), (Red, 4)]
lookupNoble 4 = [(Blue, 4), (White, 4)]

lookupNoble 5 = [(Green, 3), (Blue, 3), (Red, 3)]
lookupNoble 6 = [(Black, 4), (White, 4)]
lookupNoble 7 = [(Blue, 4), (Green, 4)]
lookupNoble 8 = [(Green, 3), (Blue, 3), (White, 3)]
lookupNoble 9 = [(Black, 3), (Red, 3), (Green, 3)]

lookupNoble _ = []
