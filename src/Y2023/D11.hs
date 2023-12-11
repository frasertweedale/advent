module Y2023.D11 (solutions) where

import Data.List (transpose)

solutions :: [IO ()]
solutions = [s1 (go1 2), s1 (go1 1000000)]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . lines

go1 :: Int -> Input -> Int
go1 n rows = sum $ fmap dist $ pairs
  where
  grid = expand rows
  pairs = locationPairs $ findGalaxies grid
  dist (cur@(y1,x1), dest@(y2,x2))
    | cur == dest
    = 0
    | y1 == y2 && x1 < x2
    = let next = (y1, x1 + 1) in step next + dist (next,dest)
    | y1 == y2
    = let next = (y1, x1 - 1) in step next + dist (next,dest)
    | x1 == x2
    = let next = (y1 + 1, x1) in step next + dist (next,dest)
    | otherwise -- different on x and y; two possible steps
    = let
        nextY = (y1 + 1, x1)
        nextX = (y1, if x1 < x2 then x1 + 1 else x1 - 1)
        stepY = step nextY
        stepX = step nextX
      in
        if stepX < stepY
          then stepX + dist (nextX,dest)
          else stepY + dist (nextY,dest)
  step (y,x) = case grid !! y !! x of 'e' -> n ; _ -> 1

type Location = (Int,Int) -- (y,x)

-- | Pairs of locations (no repeats).
-- The first location is always the "smaller".
locationPairs :: [Location] -> [(Location, Location)]
locationPairs locs = case locs of
  [] -> []
  (loc:more) -> zip (repeat loc) more <> locationPairs more

findGalaxies :: Input -> [Location]
findGalaxies rows = do
  (y,row) <- zip [0..] rows
  (x,c)   <- zip [0..] row
  case c of '#' -> pure (y,x) ; _ -> []

expand :: Input -> Input
expand = expandEmptyRows . expandEmptyColumns

expandEmptyColumns :: Input -> Input
expandEmptyColumns = transpose . expandEmptyRows . transpose

expandEmptyRows :: Input -> Input
expandEmptyRows = fmap replace
  where
  replace row
    | all (/= '#') row  = 'e' <$ row
    | otherwise         = row

type Input = [Row]
type Row = String
