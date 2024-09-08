{-# LANGUAGE TemplateHaskell #-}
module Lenses.DevelopmentLenses where

import Lens.Micro.TH
import Types.Development

makeLenses ''Development
