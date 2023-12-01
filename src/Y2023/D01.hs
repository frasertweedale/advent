module Y2023.D01 (solutions) where

import Data.Char (isDigit, ord)
import Data.List (find, isPrefixOf)
import Data.Maybe (mapMaybe)

solutions :: [IO ()]
solutions = [s1 [], s1 strDigits]

s1 :: Table -> IO ()
s1 table = getContents >>= print . go . lines
  where
  go = sum . mapMaybe extract
  extract s =
    (\hi lo -> hi * 10 + lo)
    <$> firstDig table s
    <*> firstDig (fmap (\(t,v) -> (reverse t, v)) table) (reverse s)

type Table = [(String, Int)]

strDigits :: Table
strDigits =
  [ ("one"  , 1)
  , ("two"  , 2)
  , ("three", 3)
  , ("four" , 4)
  , ("five" , 5)
  , ("six"  , 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine" , 9)
  ]

firstDig :: Table -> String -> Maybe Int
firstDig strDigs s = case s of
  [] -> Nothing
  (c:cs) ->
    case find (\(prefix, _value) -> prefix `isPrefixOf` s) strDigs of
      Nothing
        | isDigit c -> Just (ord c - 0x30)
        | otherwise -> firstDig strDigs cs
      Just (_prefix, value) -> Just value
