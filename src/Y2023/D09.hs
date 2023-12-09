module Y2023.D09 (solutions) where

import Data.Foldable (toList)

import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 = sum . fmap (last . head . extend)

-- | Generate an extended history of the line
extend :: Line -> [Line]
extend = scanr1 merge . history
  where
  merge xs diffs = head xs : zipWith (+) xs diffs

-- | Generate history for the line.
-- Output includes the initial line.
-- The final line is an infinite repetition of `0`.
--
history :: Line -> [Line]
history xs
  | all (== 0) xs = [0:xs]
  | otherwise     = xs : history (diffs xs)

-- Return line of differences
diffs :: Line -> Line
diffs (x:y:t) = y - x : diffs (y:t)
diffs _ = []

type Input = [Line]
type Line = [Int]

parseInput :: Parser Input
parseInput = toList <$> parseLine `sepBy` char '\n'
  where
  parseLine = toList <$> int `sepBy` char ' '
