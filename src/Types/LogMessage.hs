{-# LANGUAGE DeriveGeneric #-}
module Types.LogMessage (
    LogEvent(..),
) where

import GHC.Generics

import Types.Player
import Types.Action
import Lenses.PlayerLenses as P
import Lens.Micro

import qualified GameOptions as Op

data LogEvent 
    = PlayerChoseAction Player Action
    | LastRound Player
    | LogError String
    deriving (Generic)

instance Show LogEvent where
    show (PlayerChoseAction player action) = 
        player ^. P.username <> " chose action: " <> show action

    show (LastRound player) = 
        player ^. P.username <> " has surpassed " <> show Op.vpsToWin 
        <> " points. At the end of this round, the player with the most points wins."

    show (LogError err) = err


