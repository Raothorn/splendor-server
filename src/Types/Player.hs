{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Types.Player (
    Player,
    newPlayer
) where

import GHC.Generics
import qualified Data.Map as M

import Lens.Micro.Platform()

import Types.Development
import Types.GemColor
import Types.Noble


----------------------------------
-- Type
----------------------------------
data Player = Player
    { _ownedDevelopments :: [DevelopmentId]
    , _reservedDevelopments :: [DevelopmentId]
    , _tokens :: TokenPiles
    , _nobles :: [NobleId]
    , _username :: String
    , _turnOrder :: Int
    }
    deriving (Generic, Show)

----------------------------------
-- Initializer
----------------------------------
newPlayer :: String -> Int -> Player
newPlayer username order =
    let
        tokens = M.fromList $ map (,0) allColorsAndGold
    in
        Player [] [] tokens [] username order
----------------------------------
-- Lookup
----------------------------------
