module Y2023.D04 (solutions) where

import Data.Maybe (mapMaybe)
import Data.List (intersect)

import Util.Parser

solutions :: [IO ()]
solutions = [s1, s2]

s1 :: IO ()
s1 =
  getContents
  >>= print . sum . fmap (maybe 0 score . parseCard) . lines

s2 :: IO ()
s2 =
  getContents
  >>= print . sum . go (repeat (1::Int)) . mapMaybe parseCard . lines
  where
  go _ [] = []
  go [] _ = [] -- can't happen
  -- n = how many occurrences of THIS card
  -- occurs = how many occurrences of following cards
  -- card = current card
  -- deck = rest of cards
  go (n:occurs) (card:deck) =
    let
      factors' =
        zipWith (+) (replicate (numMatches card) n <> repeat 0) occurs
    in
      n : go factors' deck

score :: Card -> Int
score game = case numMatches game of
  n | n > 0 -> 2 ^ (n - 1)
  _         -> 0

numMatches :: Card -> Int
numMatches (Card _ win play) = length $ intersect win play

data Card = Card
  Int   -- card number
  [Int] -- winning numbers
  [Int] -- played numbers
  deriving (Show)

parseCard :: String -> Maybe Card
parseCard = fmap fst . runParser p
  where
  p =
    Card <$ string "Card" <* spaces
      <*> int <* char ':' <* spaces
      <*> many (int <* spaces) <* char '|' <* spaces
      <*> many (int <* spaces)
