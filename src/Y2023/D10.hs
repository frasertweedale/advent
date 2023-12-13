module Y2023.D10 (solutions) where

import Data.Maybe (fromMaybe)
import Data.List (sort)
import Data.List.NonEmpty (groupWith, toList)

import qualified Util.Map as Map
import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1, s1 go2]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 rows =
  (`div` 2)
  $ length
  $ loop (buildAdjMap rows) (startLoc rows)

go2 :: Input -> Int
go2 rows =
  sum
  $ fmap (countInside replace rows . toList)
  $ groupWith fst  -- group by row
  $ sort path
  where
    start = startLoc rows
    path = loop (buildAdjMap rows) start
    replace = ReplaceS $ whatFits start (path !! 1) (last path)

-- What piece of piece fits in location, given the
-- two adjacent locations?
--
whatFits :: Location -> Location -> Location -> Char
whatFits s l1 l2
  | lo == (ys - 1, xs) && hi == (ys + 1, xs) = '|'
  | lo == (ys, xs - 1) && hi == (ys, xs + 1) = '-'
  | lo == (ys - 1, xs) && hi == (ys, xs - 1) = 'J'
  | lo == (ys, xs + 1) && hi == (ys + 1, xs) = 'F'
  | lo == (ys, xs - 1) && hi == (ys + 1, xs) = '7'
  | lo == (ys - 1, xs) && hi == (ys, xs + 1) = 'L'
  | otherwise = error "loc not adj to s"
  where
    lo = min l1 l2
    hi = max l1 l2
    (ys,xs) = s

newtype ReplaceS = ReplaceS Char

replaceS :: ReplaceS -> Char -> Char
replaceS (ReplaceS s) c = case c of
  'S' -> s
  _   -> c

data InOut = Inside | Outside
  deriving (Eq, Show)

cross :: InOut -> InOut
cross Inside  = Outside
cross Outside = Inside

-- | Count tiles inside the path, for one row.
-- Assumes all tiles are on the same row, and are sorted
-- by x coordinate.
--
countInside :: ReplaceS -> Input -> [Location] -> Int
countInside replace rows = go Outside
  where
  go inout row =
    let
      (maybeCross, rest) = crossOrNot replace rows row
    in
      case rest of
        ((_,x1):(y2,x2):rest') ->
          case maybeCross inout of
            Inside  -> x2 - 1 - x1 + go Inside  ((y2,x2):rest')
            Outside ->               go Outside ((y2,x2):rest')
        _ ->
          0  -- no more path segments; we must be outside

-- | Pull connected (on row) path tiles and calculate whether we
-- crossed the path or not, and the remainder of the list.
-- Head of the remainder of the list is the last connected path
-- tile of the maybe-crossing.
--
crossOrNot :: ReplaceS -> Input -> [Location] -> (InOut -> InOut, [Location])
crossOrNot replace rows = go '.'
  where
    go _ [] = (id, [])
    go entry locs@((y1,x1):rest) =
      case replaceS replace (rows !! y1 !! x1) of
        '|' -> (cross, locs)
        'F' -> go 'F' rest
        'L' -> go 'L' rest
        'J' | entry == 'F' -> (cross, locs)
            | entry == 'L' -> (id, locs)
        '7' | entry == 'F' -> (id, locs)
            | entry == 'L' -> (cross, locs)
        _   -> go entry rest  -- must be '-'

type Location = (Int,Int) -- (y,x)
type Graph = Map.Map Location [Location]

startLoc :: Input -> Location
startLoc rows = head $ do
  (y,row) <- zip [0..] rows
  (x,c)   <- zip [0..] row
  case c of
    'S' -> pure (y,x)
    _   -> []

buildAdjMap :: Input -> Graph
buildAdjMap rows = Map.fromList $ do
  (y,row) <- zip [0..] rows
  (x,c)   <- zip [0..] row
  case c of
    '.' -> []
    '|' -> pure $ ( (y,x), [(y - 1,x),(y + 1,x)] )
    '-' -> pure $ ( (y,x), [(y,x - 1),(y,x + 1)] )
    'L' -> pure $ ( (y,x), [(y - 1,x),(y,x + 1)] )
    'J' -> pure $ ( (y,x), [(y - 1,x),(y,x - 1)] )
    '7' -> pure $ ( (y,x), [(y,x - 1),(y + 1,x)] )
    'F' -> pure $ ( (y,x), [(y,x + 1),(y + 1,x)] )
    'S' -> pure $ ( (y,x), [(y - 1,x),(y + 1,x),(y,x - 1),(y,x + 1)] )
    _   -> error "unexpected char in input"


-- Return the loop path from starting location.
-- Path starts with the start location, and does not
-- include any location more than once.
loop :: Graph -> Location -> [Location]
loop graph start =
  start : go (Map.insert start () Map.empty) (firstStep graph start)
  where
    go seen loc =
      let
        next =
          filter (\peer -> not (Map.member peer seen))
          $ fromMaybe []
          $ Map.lookup loc graph
      in
        case next of
          []        -> [loc]  -- terminating condition
          [nextLoc] -> loc : go (Map.insert loc () seen) nextLoc
          _         -> error $ "unexpected fork in the road: " <> show next

-- | The first step: filter to vertices that have an edge
-- back to `start`, then choose the first
--
firstStep :: Graph -> Location -> Location
firstStep graph start =
  head
  $ filter (\loc -> elem start $ fromMaybe [] $ Map.lookup loc graph)
  $ fromMaybe []
  $ Map.lookup start graph

type Input = [String]

parseInput :: Parser Input
parseInput = Parser $ \s -> Just (lines s, "")
