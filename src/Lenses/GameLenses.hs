{-# LANGUAGE TemplateHaskell #-}
module Lenses.GameLenses where

import Lens.Micro.TH

import Types

makeLenses ''SplendorGame
