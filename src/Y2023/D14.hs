module Y2023.D14 (solutions) where

import Data.List (sort, transpose)

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Pattern -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . lines

go1 :: Pattern -> Int
go1 = sum . fmap loadLeft . fmap tiltLeft . transpose

type Pattern = [String]

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
