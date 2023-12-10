{-# LANGUAGE ApplicativeDo #-}

module Y2023.D10 (solutions) where

import Data.Maybe (fromMaybe)

import qualified Util.Map as Map
import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 rows =
  (`div` 2)
  $ length
  $ loop (buildAdjMap rows) (startLoc rows)

type Location = (Int,Int) -- (y,x)
type Graph = Map.Map Location [Location]

startLoc :: Input -> Location
startLoc rows = head $ do
  (y,row) <- zip [0..] rows
  (x,c)   <- zip [0..] row
  case c of
    'S' -> pure (y,x)
    _   -> []

buildAdjMap :: Input -> Graph
buildAdjMap rows = Map.fromList $ do
  (y,row) <- zip [0..] rows
  (x,c)   <- zip [0..] row
  case c of
    '.' -> []
    '|' -> pure $ ( (y,x), [(y - 1,x),(y + 1,x)] )
    '-' -> pure $ ( (y,x), [(y,x - 1),(y,x + 1)] )
    'L' -> pure $ ( (y,x), [(y - 1,x),(y,x + 1)] )
    'J' -> pure $ ( (y,x), [(y - 1,x),(y,x - 1)] )
    '7' -> pure $ ( (y,x), [(y,x - 1),(y + 1,x)] )
    'F' -> pure $ ( (y,x), [(y,x + 1),(y + 1,x)] )
    'S' -> pure $ ( (y,x), [(y - 1,x),(y + 1,x),(y,x - 1),(y,x + 1)] )
    _   -> error "unexpected char in input"


-- Return the loop path from starting location.
-- Path starts with the start location, and does not
-- include any location more than once.
loop :: Graph -> Location -> [Location]
loop graph start =
  start : go (Map.insert start () Map.empty) (firstStep graph start)
  where
    go seen loc =
      let
        next =
          filter (\peer -> not (Map.member peer seen))
          $ fromMaybe []
          $ Map.lookup loc graph
      in
        case next of
          []        -> [loc]  -- terminating condition
          [nextLoc] -> loc : go (Map.insert loc () seen) nextLoc
          _         -> error $ "unexpected fork in the road: " <> show next

-- | The first step: filter to vertices that have an edge
-- back to `start`, then choose the first
--
firstStep :: Graph -> Location -> Location
firstStep graph start =
  head
  $ filter (\loc -> elem start $ fromMaybe [] $ Map.lookup loc graph)
  $ fromMaybe []
  $ Map.lookup start graph

type Input = [String]

parseInput :: Parser Input
parseInput = Parser $ \s -> Just (lines s, "")
