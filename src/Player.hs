module Player (
    zoomPlayer,
    updatePlayerTokens
) where

import Lens.Micro
import Lens.Micro.Mtl

import Types

zoomPlayer :: Guid -> Update Player () -> Update SplendorGame ()
zoomPlayer pg = zoom (sgPlayers . at pg . traversed)

updatePlayerTokens :: (Int -> Int) -> GemColor -> Update Player ()
updatePlayerTokens f color = at color . mapped %= f
