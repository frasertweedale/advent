{-# LANGUAGE ApplicativeDo #-}

module Y2023.D06 (solutions) where

import Util.Parser
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

solutions :: [IO ()]
solutions = [s1 play1]

s1 :: (Input -> Int) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

type Time = Int
type Distance = Int
type Input = NonEmpty Race
type Race = (Time, Distance)

play1 :: Input -> Int
play1 = product . fmap waysToBeatRecord

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
