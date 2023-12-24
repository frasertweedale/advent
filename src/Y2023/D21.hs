module Y2023.D21 (solutions) where

import Control.Monad (when)
import Data.Foldable (fold)
import Data.List (nub, sort)
import Data.Monoid (First(..))

import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [s1 go1, s1 go2]

s1 :: (Input -> IO ()) -> IO ()
s1 go =
  getContents
  >>= go . parseInput

go1 :: Input -> IO ()
go1 (plots, start) = print $ length $ go (64 :: Int) [start]
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

-- Not used, but keep it handy
_printGrid :: Plots -> Location -> [Location] -> String
_printGrid plots limit@(ymax,xmax) occupied = go 0 allLocs locs'
  where
  go _ [] _                 = "\n"
  go y locs@((y',_):_) heres
    | y < y'                = '\n' : go y' locs heres
  go y (loc:locs) (here:heres)
    | loc == here           = 'O' : go y locs heres
  go y (loc:locs) heres
    | Map.member loc plots  = '.' : go y locs heres
    | otherwise             = '#' : go y locs heres

  allLocs = [(y,x) | y <- [0..ymax], x <- [0..xmax]]
  locs' =
    sort
    $ filter (\loc -> locMod limit loc == loc)  -- in limits
    $ occupied

-- This is another puzzle that depends on "manual inspection"
-- of the input to find characteristics that simplify the
-- solution.  The grid forms "diamonds" - one in the middle
-- (reaching to the edge of the input), surrounded by eight
-- others (for 9 in total), then 16 more (for 25), and so
-- on.  The centre diamond is repeated above, below and
-- at the sides.  The other diamonds are repetitions of the
-- diamond formed by rearranging the four "corner" triangles
-- in the input.  There is a nice clean "gutter" of garden
-- plots around the diamonds, free of rocks.
--
-- The grid is 131 wide and 131 high, a square.  (So, the
-- "diamonds" are actually squares too, rotated 45°; but
-- they look like diamonds when I render it in my terminal).
--
-- Now to the number of steps: 26501365.  What happens when
-- you divide by the grid size?
--
-- @
-- λ> 26501365 `divMod` 131
-- (202300,65)
-- @
--
-- The first 65 takes us to the middle of the first "gutter"
-- (one diamond).  Then we step through over 202300 more
-- rows of diamonds.  The geometric series for the number of
-- diamonds is (2n+1)².  Of this number half (plus one) are
-- "centre" diamonds, and half (floor) are "corner" diamonds.
--
-- However, note that because the input has an odd dimension,
-- the "parity" switches for each "layer" of diamonds.  We
-- will define parity based on whether the "middle" plot of a
-- diamond can be occupied (assuming it is not a rock).  It is
-- EVEN parity if it can be occupied, and ODD parity if it
-- cannot.  After an ODD number of steps, the centre diamond
-- has ODD parity, the 8 surrounding it have EVEN parity, the
-- 16 in the next layer have ODD parity, and so on.
--
-- We consider the occupancy of the ever-expanding "diamond"
-- in the following way: consider the number of even and odd
-- parity "full grids", then subtract or add corners of the
-- relevant parity to obtain the final occupancy.  The number
-- of new grids /added/ at each cycle is given by:
--
--   4*n*par - (n-1)*par + n*par'
--
-- where `par` is the even grid occupancy if n is odd, or the
-- odd grid occupancy if n is even, and `par'` is the
-- occupancy of the opposite parity to `par`.
--
-- With some simplifications, we can ignore the corner
-- subtractions and additions at each step, and after the
-- final step add the central grid (which has odd parity),
-- and add and subtract the outermost corners.
--
-- So, we need to calculate two groups of values:
--
-- 1. The number of tiles occupied after 64 and 65 steps from
--    the centre.  This is the occupancy of the "centre"
--    diamond, for the even and odd parity respectively.
--
-- 2. The number of tiles occupied after 64 and >63< steps from
--    all four corners.  This is the occupancy of the "corner"
--    diamond.  We must step 63 times rather than 65 to avoid
--    overlaps with the centre diamond.
--
go2 :: Input -> IO ()
go2 (plots, start) = do
  when (ymax /= xmax ) $
    error "expected square input, but it's not"
  when ( extra /= ymax `div` 2) $
    error "unexpected number of extra steps"
  print result

  where
  result :: Int
  result =
    (+ length occOdd)
    . (\x -> x - (cycles + 1) * length occCnrOdd + cycles * length occCnrEven)
    . sum
    . fmap ring
    $ [1..cycles]

  ring :: Int -> Int
  ring n
    | odd n     = n * 4 * length occEven
    | otherwise = n * 4 * length occOdd

  (cycles, extra) = 26501365 `divMod` (ymax + 1)

  (occCtrEven, occCtrOdd) = go 65 [] startPointsMiddle
  (occCnrOdd, occCnrEven) = go 64 [] startPointsCorners
  occEven = occCtrEven <> occCnrEven
  occOdd = occCtrOdd <> occCnrOdd

  go :: Int -> [Location] -> [Location] -> ([Location], [Location])
  go n prev cur
    | n <= 0 = (prev, cur)
    | otherwise =
        let
          next =
            nub
            . filter (`Map.member` plots) -- avoid rocks
            . filter (\loc -> locMod limit loc == loc) -- don't step past limit
            $ cur >>= step
        in
          go (n - 1) cur next

  limit = maximum $ Map.keys plots
  (ymax,xmax) = limit

  startPointsMiddle   = [ start ]
  startPointsCorners  = [ (0,0), (0,xmax), (ymax,0), (ymax,xmax) ]

locMod
  :: Location  -- ^ limit
  -> Location  -- ^ current location
  -> Location  -- ^ current location, translated to within original grid
locMod (ymax,xmax) (y,x) = (y `mod` (ymax + 1), x `mod` (xmax + 1))

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
