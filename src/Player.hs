module Player (
    getDevelopmentGems,
    zoomPlayer,
    updatePlayerTokens,
) where

import Control.Monad
import qualified Data.Map as M

import Lens.Micro
import Lens.Micro.Mtl

import Types
import Util
import DevelopmentLookup 

-----------------------------------------
-- Normal functions (for serialization)
-----------------------------------------
getDevelopmentGems :: Player -> TokenPiles
getDevelopmentGems player = M.fromList $ map (\c -> (c, getGemAmt c)) allColors
    where 
        developments = map getDevelopmentData (player ^. pOwnedDevelopments)
        getGemAmt c = length (filter (\d -> developmentGem d == c) developments) 
            
----------------------------------
-- Stateful functions
----------------------------------
zoomPlayer :: Guid -> Update Player () -> Update SplendorGame ()
zoomPlayer pg = zoom (sgPlayers . at pg . traversed)

updatePlayerTokens :: (Int -> Int) -> GemColor -> Update Player ()
updatePlayerTokens f color = do
    amount <- useEither "Cannot find tokens" (pTokens . at color)
    let amount' = f amount

    when (amount' < 0) $ liftErr notEnoughErr
    pTokens . at color . mapped .= amount'
  where
    notEnoughErr =
        "You do not have enough "
            <> show color
            <> " tokens to do that."
