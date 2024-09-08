{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Lenses.PlayerLenses where

import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Platform()

import Types.Player
import Types.GemColor
import Types.Development
import Util

import qualified Lenses.DevelopmentLenses as D

makeLenses ''Player

tokenGems :: GemColor -> SimpleGetter Player Int
tokenGems color = tokens . at color . toDefault 0

bonusGems :: GemColor -> SimpleGetter Player Int
bonusGems color = allBonusGems . at color . toDefault 0

allBonusGems :: SimpleGetter Player TokenPiles
allBonusGems = to getGems
    where
        developments p = p ^. ownedDevelopments ^.. each . lookupDev
        getGemAmt p c = length (filter (\d -> d ^. D.gem == c) (developments p))
        getGems p = M.fromList $ map (\c -> (c, getGemAmt p c)) allColors

victoryPoints :: SimpleGetter Player Int
victoryPoints = to devVps
  where
    devVps player = 
        sum $ player ^. ownedDevelopments ^.. each . lookupDev ^.. each . D.pp

-- This isn't a lens but there isn't a better place for it for now
canAfford :: DevelopmentId -> Player -> Bool
canAfford devId player =
    let cost = devId ^. lookupDev . D.cost
        calcRemaining (color, amt) =
            max 0 $ amt - (player ^. tokenGems color) - (player ^. bonusGems color)
        remaining = map calcRemaining cost
    in sum remaining <= player ^. tokenGems Gold
