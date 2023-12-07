{-# LANGUAGE ApplicativeDo #-}

module Y2023.D05 (solutions) where

import Data.Char (isAlpha)
import Data.Foldable (toList, foldl')
import Data.List (sort, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))

import Control.Parallel.Strategies

import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1, s1 go2, s1 go1', s1 goPar]

s1 :: (Input -> Int) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 (seeds, maps) =
  minimum $ foldl step seeds maps
  where
    step xs (_, rangeMaps) = fmap (next rangeMaps) xs
    next rangeMaps x =
      fromMaybe x . getFirst $ foldMap (First . doRangeMap x) rangeMaps

go1' :: Input -> Int
go1' (seeds, maps) =
  minimum $ foldl step (pairUp (toList seeds) >>= expandRange) maps
  where
    expandRange (lo,size) = take size [lo..]
    step xs (_, rangeMaps) = fmap (next rangeMaps) xs
    next rangeMaps x =
      fromMaybe x . getFirst $ foldMap (First . doRangeMap x) rangeMaps

goPar :: Input -> Int
goPar (seeds, maps) =
  minimum
    $ withStrategy (parBuffer 500 rseq)
    $ fmap (minimum . fmap trace)
    $ chunk 10000 allSeeds
  where
    allSeeds = pairUp (toList seeds) >>= expandRange
    expandRange (lo,size) = take size [lo..]

    trace seed = foldl' next seed $ fmap snd maps
    next x rangeMaps =
      fromMaybe x . getFirst $ foldMap (First . doRangeMap x) rangeMaps

    chunk _ [] = []
    chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs

doRangeMap :: Int -> RangeMap -> Maybe Int
doRangeMap x (dst0, src0, size)
  | x >= src0 && x < src0 + size  = Just $ dst0 + (x - src0)
  | otherwise                     = Nothing

go2 :: Input -> Int
go2 (seeds, maps) =
  minimum . fmap fst $ foldl step (pairUp (toList seeds)) maps
  where
    step seedRanges (_, rangeMaps) =
      mapRanges
        (sort seedRanges)
        (sortOn (\(_,srcLo,_)->srcLo) $ toList rangeMaps)
              -- ^ sort by source range lower boundary

pairUp :: [a] -> [(a,a)]
pairUp (x:y:xs) = (x,y) : pairUp xs
pairUp _ = []

-- Map ranges to ranges.  Inputs are assumed sorted.
mapRanges :: [(Int,Int)] -> [RangeMap] -> [(Int,Int)]
mapRanges [] _ = []
mapRanges l [] = l
mapRanges inSpans@((inLo,inSize):inTail) rangeMap@((dstLo,srcLo,mapSize):mapTail)
  | inLo < srcLo =
      -- pass through the non-overlapping prefix (if any)
      -- Examples: iiii   | iiii
      --             rrrr |     rrrr
      let
        preLo = inLo
        preSize = min inSize (srcLo - inLo)
        pre = (preLo,preSize)
        restLo = preLo + preSize
        restSize = inSize - preSize
        rest = (restLo,restSize)
      in
        pre : mapRanges (if restSize == 0 then inTail else rest:inTail) rangeMap
  | inLo < srcLo + mapSize =
      -- map the overlapping segment
      -- Examples: iiii |   iiii | iiii |   ii   --input
      --           rr   | rrrr   | rrrr | rrrrrr --range
      --           ss   |   ss   | ssss |   ss   --seg
      --             oo |     oo |      |        --rest
      --
      let
        segLo = max srcLo inLo
        segSize = min (inLo + inSize) (srcLo + mapSize) - segLo
        segMapped = (segLo - srcLo + dstLo, segSize)
        restLo = segLo + segSize
        restSize = inSize - segSize
        rest = (restLo,restSize)
      in
        segMapped
          : mapRanges
              (if restSize == 0 then inTail else rest:inTail)
              (if inLo + inSize < srcLo + mapSize then rangeMap else mapTail)
  | otherwise =
      -- inLo >= srcLo + mapSize.  Drop this map and recurse.
      mapRanges inSpans mapTail

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
