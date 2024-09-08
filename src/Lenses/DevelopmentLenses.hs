{-# LANGUAGE TemplateHaskell #-}
module Lenses.DevelopmentLenses where

import Lens.Micro
import Lens.Micro.TH
import Types.Development

makeLenses ''Development

unshownDevs :: Lens' DevelopmentDeck [DevelopmentId]
unshownDevs = _1

shownDevs :: Lens' DevelopmentDeck [DevelopmentId]
shownDevs = _2
