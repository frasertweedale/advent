module Y2023.D13 (solutions) where

import Data.List (elemIndex, transpose)

solutions :: [IO ()]
solutions = [s1 (go1 Nothing), s1 (go1 (Just smudge))]

s1 :: (Show a) => ([Pattern] -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . parseInput

go1 :: Maybe Smudge -> [Pattern] -> Int
go1 f = sum . fmap (reflSum f)

reflSum :: Maybe Smudge -> Pattern -> Int
reflSum f pat = sum (reflVert f pat) + 100 * sum (reflHoriz f pat)

-- | Function that returns possible smudges of a line
type Smudge = String -> [String]

-- | Smudge a line
smudge :: Smudge
smudge ""     = []
smudge (c:cs) = (opposite c : cs) : fmap (c:) (smudge cs)

opposite :: Char -> Char
opposite = \case
  '.' -> '#'
  '#' -> '.'
  c   -> c


reflVert :: Maybe Smudge -> Pattern -> [Int]
reflVert f = reflHoriz f . transpose

reflHoriz :: Maybe Smudge -> Pattern -> [Int]
reflHoriz f = go 0 []
  where
  go n [] (x:xs) = go (n + 1) [x] xs
  go _ _  [] = []
  go n l r@(rh:rt)
    | isRefl f (zip l r)  = [n]
    | otherwise           = go (n + 1) (rh:l) rt

-- Smudge, if given, MUST be applied.
--
isRefl :: Maybe Smudge -> [(String, String)] -> Bool
isRefl mf = go mf
  where
  go f []                                 = maybe True (const $ null f) mf
  go f ((l,r):more)
    | l == r                              = isRefl f more
    | maybe False (any (== r) . ($ l)) f  = isRefl Nothing more
    | otherwise                           = False

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
