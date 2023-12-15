module Y2023.D15 (solutions) where

import Data.Char (ord)
import Data.Word (Word8)
import Data.List.NonEmpty (toList)

import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 = sum . fmap (fromIntegral . hash)

hash :: String -> Word8
hash = foldl step 0
  where
  step acc c = (acc + fromIntegral (ord c)) * 17

type Input = [String]

parseInput :: Parser Input
parseInput =
  fmap (filter (/= '\n')) . toList
  <$> many (satisfy (/= ',')) `sepBy` char ','
