module DevelopmentLookup (
    getDevelopmentData,
    getGemCost,
    getDeckIndex
) where

import Data.Maybe

import Types

getDevelopmentData :: DevelopmentId -> Development
getDevelopmentData devId =
    let
        (gem, vp, costs) = gdd devId
        costs' = zip allColors costs
     in
        Development gem costs' vp devId

getDeckIndex :: DevelopmentId -> Int
getDeckIndex n
    | n <= 40 = 0
    | n <= 70 = 1
    | otherwise = 2

getGemCost :: GemColor -> Development -> Int
getGemCost color development = fromMaybe 0 $ lookup color (developmentCost development)

gdd :: DevelopmentId -> (GemColor, Int, [Int])
gdd 1 = (Red, 0, [3, 0, 0, 0, 0])
gdd 2 = (Red, 0, [2, 1, 1, 0, 1])
gdd 3 = (White, 0, [0, 0, 0, 2, 1])
gdd 4 = (White, 0, [0, 1, 1, 1, 1])
gdd 5 = (White, 0, [3, 1, 0, 0, 1])
gdd 6 = (Green, 0, [1, 1, 0, 1, 2])
gdd 7 = (Green, 0, [2, 1, 0, 0, 0])
gdd 8 = (Blue, 0, [1, 0, 0, 0, 2])
gdd 9 = (Black, 0, [0, 0, 3, 0, 0])
gdd 10 = (Black, 0, [1, 2, 1, 1, 0])
gdd 11 = (White, 0, [0, 1, 2, 1, 1])
gdd 12 = (White, 0, [0, 2, 0, 0, 2])
gdd 13 = (Blue, 0, [0, 0, 0, 4, 0])
gdd 14 = (Blue, 0, [0, 0, 2, 0, 2])
gdd 15 = (Blue, 0, [1, 0, 1, 1, 1])
gdd 16 = (Black, 0, [2, 2, 0, 1, 0])
gdd 17 = (Black, 0, [0, 4, 0, 0, 0])
gdd 18 = (Black, 0, [0, 0, 2, 1, 0])
gdd 19 = (Red, 0, [2, 0, 1, 0, 2])
gdd 20 = (Red, 1, [4, 0, 0, 0, 0])
gdd 21 = (Green, 0, [0, 1, 0, 2, 2])
gdd 22 = (Black, 0, [2, 0, 2, 0, 0])
gdd 23 = (Green, 0, [1, 3, 1, 0, 0])
gdd 24 = (Green, 0, [0, 0, 0, 3, 0])
gdd 25 = (White, 1, [0, 0, 4, 0, 0])
gdd 26 = (Red, 0, [1, 1, 1, 0, 1])
gdd 27 = (Blue, 0, [1, 0, 1, 2, 1])
gdd 28 = (Blue, 0, [0, 1, 3, 1, 0])
gdd 29 = (Green, 0, [1, 1, 0, 1, 1])
gdd 30 = (Green, 0, [0, 2, 0, 2, 0])
gdd 31 = (Green, 1, [0, 0, 0, 0, 4])
gdd 32 = (Blue, 0, [1, 0, 2, 2, 0])
gdd 33 = (Blue, 0, [0, 0, 0, 0, 3])
gdd 34 = (White, 0, [0, 3, 0, 0, 0])
gdd 35 = (White, 0, [0, 2, 2, 0, 1])
gdd 36 = (Red, 0, [1, 0, 0, 1, 3])
gdd 37 = (Red, 0, [0, 2, 1, 0, 0])
gdd 38 = (Black, 0, [1, 1, 1, 1, 0])
gdd 39 = (Black, 0, [0, 0, 1, 3, 1])
gdd 40 = (Red, 0, [2, 0, 0, 2, 0])
gdd 41 = (Red, 2, [3, 0, 0, 0, 5])
gdd 42 = (Blue, 3, [0, 6, 0, 0, 0])
gdd 43 = (Green, 3, [0, 0, 6, 0, 0])
gdd 44 = (White, 3, [6, 0, 0, 0, 0])
gdd 45 = (Black, 1, [3, 0, 3, 0, 2])
gdd 46 = (Green, 2, [0, 0, 5, 0, 0])
gdd 47 = (Black, 2, [0, 0, 5, 3, 0])
gdd 48 = (Red, 1, [2, 0, 0, 2, 3])
gdd 49 = (Black, 2, [0, 1, 4, 2, 0])
gdd 50 = (Green, 1, [3, 0, 2, 3, 0])
gdd 51 = (White, 2, [0, 0, 0, 5, 0])
gdd 52 = (Red, 3, [0, 0, 0, 6, 0])
gdd 53 = (White, 2, [0, 0, 1, 4, 2])
gdd 54 = (Red, 2, [0, 0, 0, 0, 5])
gdd 55 = (White, 1, [0, 0, 3, 2, 2])
gdd 56 = (Green, 2, [4, 2, 0, 0, 1])
gdd 57 = (Blue, 2, [5, 3, 0, 0, 0])
gdd 58 = (Black, 3, [0, 0, 0, 0, 6])
gdd 59 = (Green, 2, [0, 5, 3, 0, 0])
gdd 60 = (Blue, 1, [0, 2, 3, 0, 3])
gdd 61 = (Black, 2, [5, 0, 0, 0, 0])
gdd 62 = (Blue, 2, [2, 0, 0, 1, 4])
gdd 63 = (Blue, 1, [0, 2, 2, 3, 0])
gdd 64 = (White, 2, [0, 0, 0, 5, 3])
gdd 65 = (White, 1, [2, 3, 0, 3, 0])
gdd 66 = (Red, 2, [1, 4, 2, 0, 0])
gdd 67 = (Black, 1, [3, 2, 2, 0, 0])
gdd 68 = (Red, 1, [0, 3, 0, 2, 3])
gdd 69 = (Blue, 2, [0, 5, 0, 0, 0])
gdd 70 = (Green, 1, [2, 3, 0, 0, 2])
gdd 71 = (Black, 4, [0, 0, 0, 7, 0])
gdd 72 = (Red, 5, [0, 0, 7, 3, 0])
gdd 73 = (Green, 4, [0, 7, 0, 0, 0])
gdd 74 = (White, 4, [3, 0, 0, 3, 6])
gdd 75 = (Black, 3, [3, 3, 5, 3, 0])
gdd 76 = (Black, 4, [0, 0, 3, 6, 3])
gdd 77 = (White, 4, [0, 0, 0, 0, 7])
gdd 78 = (Green, 4, [3, 6, 3, 0, 0])
gdd 79 = (Blue, 5, [7, 3, 0, 0, 0])
gdd 80 = (Red, 4, [0, 3, 6, 3, 0])
gdd 81 = (Red, 4, [0, 0, 7, 0, 0])
gdd 82 = (Black, 5, [0, 0, 0, 7, 3])
gdd 83 = (Green, 5, [0, 7, 3, 0, 0])
gdd 84 = (Green, 3, [5, 3, 0, 3, 3])
gdd 85 = (Blue, 4, [6, 3, 0, 0, 3])
gdd 86 = (White, 5, [3, 0, 0, 0, 7])
gdd 87 = (Blue, 4, [7, 0, 0, 0, 0])
gdd 88 = (Red, 3, [3, 5, 3, 0, 3])
gdd 89 = (White, 3, [0, 3, 3, 5, 3])
gdd 90 = (Blue, 3, [3, 0, 3, 3, 5])
gdd _ = (White, 0, [0, 0, 0, 0, 0])
