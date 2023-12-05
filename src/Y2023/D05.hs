{-# LANGUAGE ApplicativeDo #-}

module Y2023.D05 (solutions) where

import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))

import Util.Parser

solutions :: [IO ()]
solutions = [s1]

s1 :: IO ()
s1 =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go :: Input -> Int
go (seeds, maps) =
  minimum $ foldl step seeds maps
  where
    step xs (_, rangeMaps) = fmap (next rangeMaps) xs
    next rangeMaps x =
      fromMaybe x . getFirst $ foldMap (First . doRangeMap x) rangeMaps

doRangeMap :: Int -> RangeMap -> Maybe Int
doRangeMap x (dst0, src0, size)
  | x >= src0 && x < src0 + size  = Just $ dst0 + (x - src0)
  | otherwise                     = Nothing

type Input = (NonEmpty Seed, NonEmpty ResourceMap)
type Seed = Int
type Resource = String
type ResourceMap = ((Resource, Resource), NonEmpty RangeMap)
type RangeMap =
  ( Int -- dst range start
  , Int -- src range start
  , Int -- range size
  )

parseInput :: Parser Input
parseInput = do
  seeds <- string "seeds: " *> int `sepBy` spaces
  _ <- string "\n\n"
  resourceMaps <- parseResourceMap `sepBy` string "\n\n"
  pure (seeds, resourceMaps)

parseResourceMap :: Parser ResourceMap
parseResourceMap = do
  res1 <- some (satisfy isAlpha)
  _ <- string "-to-"
  res2 <- some (satisfy isAlpha)
  _ <- string " map:" <* char '\n'
  rangeMaps <- parseRangeMap `sepBy` char '\n'
  pure ((res1, res2), rangeMaps)

parseRangeMap :: Parser RangeMap
parseRangeMap = do
  dst0 <- int
  _ <- char ' '
  src0 <- int
  _ <- char ' '
  size <- int
  pure (dst0, src0, size)
