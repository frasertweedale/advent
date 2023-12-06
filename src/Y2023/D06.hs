{-# LANGUAGE ApplicativeDo #-}

module Y2023.D06 (solutions) where

import Util.Parser
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

solutions :: [IO ()]
solutions = [s1 parseInput, s1 parseInput2]

s1 :: Parser Input -> IO ()
s1 parser =
  getContents
  >>= print . play . maybe (error "parse failed") fst . runParser parser

type Time = Int
type Distance = Int
type Input = NonEmpty Race
type Race = (Time, Distance)

play :: Input -> Int
play = product . fmap waysToBeatRecord

waysToBeatRecord :: Race -> Int
waysToBeatRecord (t, dist) =
  length $ filter beats [1..t-1]
  where
    beats heldFor = heldFor * (t - heldFor) > dist

parseInput :: Parser Input
parseInput = do
  times <- string "Time:" *> spaces *> int `sepBy` spaces
  _ <- char '\n'
  distances <- string "Distance:" *> spaces *> int `sepBy` spaces
  pure $ NE.zip times distances

parseInput2 :: Parser Input
parseInput2 = do
  times <- string "Time:" *> spaces *> int `sepBy` spaces
  _ <- char '\n'
  distances <- string "Distance:" *> spaces *> int `sepBy` spaces
  pure $
    let
      time = read $ foldr ((<>) . show) "" times
      distance = read $ foldr ((<>) . show) "" distances
    in
      pure (time, distance)
