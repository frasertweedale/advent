module Y2023.D11 (solutions) where

import Data.List (transpose)

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . lines

go1 :: Input -> Int
go1 = sum . fmap (uncurry steps) . locationPairs . findGalaxies . expand

type Location = (Int,Int) -- (y,x)

-- | How many steps between locations?
steps :: Location -> Location -> Int
steps (y1,x1) (y2,x2) = abs (y2 - y1) + abs (x2 - x1)

-- | Pairs of locations (no repeats)
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
expandEmptyRows rows = do
  row <- rows
  if all (== '.') row
    then [row,row]
    else [row]

type Input = [Row]
type Row = String
