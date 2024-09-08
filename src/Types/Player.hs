{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Types.Player (
    Player,
    newPlayer
) where

import GHC.Generics
import qualified Data.Map as M

import Types.Development
import Types.GemColor


----------------------------------
-- Type
----------------------------------
data Player = Player
    { _ownedDevelopments :: [DevelopmentId]
    , _reservedDevelopments :: [DevelopmentId]
    , _tokens :: TokenPiles
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
        Player [] [] tokens username order

----------------------------------
-- Lookup
----------------------------------
