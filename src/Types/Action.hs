{-# LANGUAGE DeriveGeneric #-}
module Types.Action (
    Action(..)
) where

import GHC.Generics

import Types.GemColor
import Types.Development


data Action
    = NoAction
    | AcquireTokens [GemColor]
    | PurchaseDevelopment DevelopmentId [(GemColor, Int)]
    | ReserveDevelopment DevelopmentId
    deriving (Generic, Show)
