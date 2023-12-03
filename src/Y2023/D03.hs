module Y2023.D03 (solutions) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

import Util.Parser

solutions :: [IO ()]
solutions = [s1]

s1 :: IO ()
s1 = getContents >>= print . sumPartNumbers . lines

type Location =
  ( Int {- y (line number) -}
  , Int {- x (offset in line) -}
  , Int {- span -}
  )

sumPartNumbers :: [String] -> Int
sumPartNumbers matrix = do
  sum $ mapMaybe isPartNumber (numbersWithLocation matrix)
  where
    isPartNumber :: (Int, Location) -> Maybe Int
    isPartNumber (val, loc) =
      if locValid matrix loc
        then Just val
        else Nothing

numbersWithLocation :: [String] -> [(Int, Location)]
numbersWithLocation matrix =
  concat $ fmap getNumbersWithLocation (zip [0..] matrix)

locValid :: [String] -> Location -> Bool
locValid matrix loc =
  any (locIsSymbol matrix) (surroundingCoords loc)

locIsSymbol :: [String] -> Location -> Bool
locIsSymbol matrix loc =
  let c = getCharAt matrix loc
  in not (isDigit c || c == '.')

getCharAt :: [String] -> Location -> Char
getCharAt matrix (y,x,_) =
  if y < 0 || y >= length matrix
    then '.'
    else
      let line = matrix !! y
      in
        if x < 0 || x >= length line
          then '.'
          else line !! x


getNumbersWithLocation :: (Int, String)  -> [(Int, Location)]
getNumbersWithLocation (y,str) =
  doit 0 str
  where
    doit _ "" = []
    doit off s = case runParser int s of
      Nothing
        -> doit (off + 1) (drop 1 s)
      Just (intVal, remainder)
        -> (intVal, (y, off, numLen)) : doit newOff remainder
          where
          numLen = length s - length remainder
          newOff = off + numLen

-- | Return a list of (y,x,_) locations surround the given location.
-- The 'span' part is irrelevant and should be ignored; all (y,x) locations
-- surrounding the given span will be returned.
--
surroundingCoords :: Location -> [Location]
surroundingCoords (y,x,spam) =
  ( do
      y' <- [y - 1, y + 1]
      x' <- [x - 1..x + spam]
      pure (y', x', 0)
  )
  ++
  [ (y, x-1, 0), (y, x + spam, 0) ]
