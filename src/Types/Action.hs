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
    deriving (Generic)

{- FOURMOLU_DISABLE -}
instance Show Action where
    show (AcquireTokens colors) =
        case colors of 
            [a, b, c] -> 
                "Aquire 1 " <> show a <> " token, 1 " <>
                show b <> " token, and 1" <> show c <> " token."
            [a] -> "Acquire 2 " <> show a <> " tokens."
            _ -> ""

    show (PurchaseDevelopment devId _) =
        "Purchase [d|" <> show devId <> "]"

    show (ReserveDevelopment devId) = 
        "Reserve [d|" <> show devId <> "]"

    show _ = "NOT IMPLEMENTED"
{- FOURMOLU_ENABLE -}
