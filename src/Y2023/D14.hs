module Y2023.D14 (solutions) where

import Data.List (sort, transpose)

import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [s1 go1, s1 go2]

s1 :: (Show a) => (Pattern -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . lines

go1 :: Pattern -> Int
go1 = sum . extractLoads . fmap tiltLeft . orient

go2 :: Pattern -> Int
go2 = sum . extractLoads . go Map.empty . zip [0..] . every4th . tilts . orient
  where
  go _ [] = error "can't happen"
  go seen ((i, pat):more) = case Map.lookup pat seen of
    Nothing -> go (Map.insert pat i seen) more
    Just i0 ->
      let
        cycleLength = i - i0
        spinsToGo = 1000000000 - i
        stepsToGo = spinsToGo `mod` cycleLength
      in
        case stepsToGo of
          0 -> pat
          _ -> snd $ more !! (stepsToGo - 1)


-- | Yield every 4th value, starting from the first.
every4th :: [a] -> [a]
every4th [] = []
every4th (x:xs) = x : every4th (drop 3 xs)

-- | Iteratively tilt THEN rotate.  A pre-rotation may be required
-- to put the pattern in the correct starting state.
--
-- The initial state IS included in the output.
--
tilts :: Pattern -> [Pattern]
tilts = iterate (rotr . fmap tiltLeft)

type Pattern = [String]

orient, rotr :: Pattern -> Pattern
orient = reverse . transpose
rotr = fmap reverse . transpose

-- | Tilt the row left ("East") and slide them rocks
tiltLeft :: String -> String
tiltLeft = \case
  ""      -> ""
  ('#':s) -> '#' : tiltLeft s
  s       -> let (l,r) = span (/= '#') s in reverse (sort l) <> tiltLeft r

-- | Calculate load on the left (single row).
loadLeft :: String -> Int
loadLeft =
  sum
  . fmap (\(i,c) -> case c of 'O' -> i ; _ -> 0)
  . zip [1..]
  . reverse

-- | Extract list of row-wise load values
extractLoads :: Pattern -> [Int]
extractLoads = fmap loadLeft
