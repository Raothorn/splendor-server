{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.Development (
    Development,
    DevelopmentDeck,
    DevelopmentId,
    lookupDeck,
    lookupDev,
    shownDevs,
    unshownDevs,
) where

import GHC.Generics (Generic)

import qualified Data.List as L

import Lens.Micro
import Lens.Micro.TH

import Types.GemColor
import DevelopmentLookup
import Util


----------------------------------
--Type 
----------------------------------
data Development = Development
    { _gem :: GemColor
    , _cost :: [(GemColor, Int)]
    , _pp :: Int
    , _id :: DevelopmentId
    }
    deriving (Generic, Show)

----------------------------------
-- Aliases 
----------------------------------
-- The first value in the tuple is the unshown cards, the second is the shown
type DevelopmentDeck = ([DevelopmentId], [DevelopmentId])

type DevelopmentId = Int

----------------------------------
-- Lenses 
----------------------------------
makeLenses ''Development
unshownDevs :: Lens' DevelopmentDeck [DevelopmentId]
unshownDevs = _1

shownDevs :: Lens' DevelopmentDeck [DevelopmentId]
shownDevs = _2
----------------------------------
-- Lookup 
----------------------------------
lookupDeck :: DevelopmentId -> Int
lookupDeck n
    | n <= 40 = 0
    | n <= 70 = 1
    | otherwise = 2

lookupDev :: SimpleGetter DevelopmentId Development
lookupDev = to lookupDev'

lookupDev' :: DevelopmentId -> Development
lookupDev' devId = Development devGem colorCost devPp devId
    where 
        (devGem, devPp, devCost) = devData devId
        colorCost = zip allColors devCost
        
getGemCost :: GemColor -> DevelopmentId -> Int
getGemCost color devId = development ^. cost . to (L.lookup color) . toDefault 0
    where development = devId ^. lookupDev
