module Y2023.D16 (solutions) where

import Data.List.NonEmpty (groupWith)

import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [s1, s2]

s1 :: IO ()
s1 =
  getContents
  >>= print . (\g -> energised g ((0,0), R)) . parseInput

s2 :: IO ()
s2 = do
  s <- getContents
  let
    g = parseInput s
  print $ maximum $ fmap (energised g) (starts s)

starts :: String -> [(Location, Direction)]
starts s =
  fmap (\y -> ((y, 0), R)) [0..nRows - 1]
  <> fmap (\y -> ((y, nCols - 1), L)) [0..nRows - 1]
  <> fmap (\x -> ((0, x), D)) [0..nCols - 1]
  <> fmap (\x -> ((nRows - 1, x), U)) [0..nCols - 1]
  where
  rows = lines s
  nRows = length rows
  row = head rows
  nCols = length row

energised :: Graph -> (Location, Direction) -> Int
energised g start = length . groupWith fst . Map.keys $ dfs start Map.empty
  where
  dfs
    :: (Location, Direction)
    -> Map.Map (Location, Direction) ()
    -> Map.Map (Location, Direction) ()
  dfs locDir seen = case Map.lookup locDir seen of
    Just _ ->
      seen
    Nothing ->
      case Map.lookup locDir g of
        Nothing -> seen
        Just xs -> foldr dfs (Map.insert locDir () seen) xs

type Graph = Map.Map (Location, Direction) [(Location, Direction)]
type Location = (Int,Int) -- ^ (y,x)
data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

parseInput :: String -> Graph
parseInput s = Map.fromList $ cells >>= uncurry edges
  where
  cells :: [(Location, Char)]
  cells = do
    (y,row) <- zip [0..] $ lines s
    (x,c) <- zip [0..] row
    pure ((y,x), c)

  -- given a location (and its symbol), give a list of outbound edges
  -- per inbound direction.
  --
  edges
    :: Location
    -> Char
    -> [ ( (Location, Direction), [(Location, Direction)] ) ]
  edges loc@(y,x) c = case c of
    '.'  -> [ ( (loc, L), [((y,x - 1), L)] )
            , ( (loc, R), [((y,x + 1), R)] )
            , ( (loc, U), [((y - 1,x), U)] )
            , ( (loc, D), [((y + 1,x), D)] )
            ]
    '/'  -> [ ( (loc, L), [((y + 1,x), D)] )
            , ( (loc, R), [((y - 1,x), U)] )
            , ( (loc, U), [((y,x + 1), R)] )
            , ( (loc, D), [((y,x - 1), L)] )
            ]
    '\\' -> [ ( (loc, L), [((y - 1,x), U)] )
            , ( (loc, R), [((y + 1,x), D)] )
            , ( (loc, U), [((y,x - 1), L)] )
            , ( (loc, D), [((y,x + 1), R)] )
            ]
    '|'  -> [ ( (loc, L), [((y - 1,x), U), ((y + 1,x), D)] )
            , ( (loc, R), [((y - 1,x), U), ((y + 1,x), D)] )
            , ( (loc, U), [((y - 1,x), U)] )
            , ( (loc, D), [((y + 1,x), D)] )
            ]
    '-'  -> [ ( (loc, L), [((y,x - 1), L)] )
            , ( (loc, R), [((y,x + 1), R)] )
            , ( (loc, U), [((y,x - 1), L), ((y,x + 1), R)] )
            , ( (loc, D), [((y,x - 1), L), ((y,x + 1), R)] )
            ]
    _    -> []  -- can't happen
