module Y2023.D13 (solutions) where

import Data.List (elemIndex, transpose)

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => ([Pattern] -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . parseInput

go1 :: [Pattern] -> Int
go1 = sum . fmap reflSum

reflSum :: Pattern -> Int
reflSum pat = sum (reflVert pat) + 100 * sum (reflHoriz pat)

reflVert :: Pattern -> [Int]
reflVert = reflHoriz . transpose

reflHoriz :: Pattern -> [Int]
reflHoriz = go 0 []
  where
  go n [] (x:xs) = go (n + 1) [x] xs
  go _ _  [] = []
  go n l r@(rh:rt)
    | all (uncurry (==)) (zip l r)  = [n]
    | otherwise                     = go (n + 1) (rh:l) rt

type Pattern = [String]

parseInput :: String -> [Pattern]
parseInput = go . lines
  where
  go [] = []
  go xs =
    case elemIndex "" xs of
      Nothing -> [xs]
      Just i ->
        let (block, rest) = splitAt i xs
        in block : go (drop 1 rest)
