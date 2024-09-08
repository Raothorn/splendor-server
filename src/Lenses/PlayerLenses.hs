{-# LANGUAGE TemplateHaskell #-}
module Lenses.PlayerLenses where

import Lens.Micro.TH

import Types

makeLenses ''Player
