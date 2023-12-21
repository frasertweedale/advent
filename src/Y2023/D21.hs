module Y2023.D21 (solutions) where

import Data.Foldable (fold)
import Data.List (nub)
import Data.Monoid (First(..))

import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . parseInput

go1 :: Input -> Int
go1 (plots, start) = length $ go (64 :: Int) [start]
  where
  go n locs
    | n <= 0
    = locs
    | otherwise
    = go (n - 1) $ filter (`Map.member` plots) . nub $ locs >>= step

-- | All steps (up, down, left, right)
step :: Location -> [Location]
step (y,x) =
  [ (y - 1, x)
  , (y + 1, x)
  , (y, x - 1)
  , (y, x + 1)
  ]

type Input = (Plots, Location) -- ^ Plot map, start loc

type Location = (Int, Int) -- ^ (y,x)
type Plots = Map.Map Location ()

parseInput :: String -> Input
parseInput s =
  case mStart of
    Nothing     -> error "no start location"
    Just start  -> ( Map.fromList (fmap (,()) locs), start )
  where
    (locs, First mStart) = fold $ do
      (y, line) <- zip [0..] (lines s)
      (x, c) <- zip [0..] line
      case c of
        '.' -> [ ( [(y,x)], mempty ) ]
        'S' -> [ ( [(y,x)], First (Just (y,x)) ) ]
        _   -> []
