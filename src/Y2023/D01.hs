module Y2023.D01 (solutions) where

import Data.Char
import Data.Monoid
import Data.Maybe (mapMaybe)

solutions :: [IO ()]
solutions = [s1]

s1 :: IO ()
s1 = getContents >>= print . go . lines
  where
  go = sum . mapMaybe extract
  extract s = (\hi lo -> hi * 10 + lo) <$> firstDig s <*> lastDig s
  firstDig = getFirst . foldMap (First . toDigit)
  lastDig = getLast . foldMap (Last . toDigit)
  toDigit c
    | isDigit c = Just (ord c - 0x30)
    | otherwise = Nothing
