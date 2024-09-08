module Types.Alias where

import Control.Monad.Trans.State.Lazy

type Update s a = StateT s (Either String) a

type Guid = String

type GameMessage = String
type GameOverSummary = String
