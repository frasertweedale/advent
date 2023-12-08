{-# LANGUAGE ApplicativeDo #-}

module Y2023.D08 (solutions) where

import Data.Char (isAlphaNum)
import Data.Foldable (toList)

import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1, s1 go2]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 (path, nodes) = followPath "AAA" (cycle path)
  where
  followPath "ZZZ" _ = 0
  followPath _ [] = 0
  followPath loc (dir:dirs) = 1 + case lookup loc nodes of
    Nothing -> 0
    Just (l,r) -> followPath (case dir of L -> l ; R -> r) dirs


--                      ┌───  A to Z₁ ──┐ ┌──  Z₁ to Z₂ ──┐┌Z₁=Z₂┐
go2 :: Input -> (Int, [(Int,Bool,Location,Int,Bool,Location,Bool)])
go2 (path, nodes) = (n, results)
  where
    -- This outputs the lengths of the paths from the A node to
    -- the first Z node; and again to the next Z node, verifies
    -- that these lengths are a multiple of the path length,
    -- and verifies that the first Z node and second Z node are the
    -- same node.
    f node =
      let
        firstCycle = cycleToZ nodes (cycle path) node
        lenFirstCycle = length $ drop 1 firstCycle
        (midLoc, midDirs) = last firstCycle
        secondCycle = cycleToZ nodes midDirs midLoc
        lenSecondCycle = length $ drop 1 firstCycle
        (endLoc, _) = last secondCycle
      in
        ( lenFirstCycle, lenFirstCycle `mod` length path == 0, midLoc
        , lenSecondCycle, lenSecondCycle `mod` length path == 0, endLoc
        , midLoc == endLoc
        )
    results = fmap f startNodes
    n = foldr lcm 1 $ fmap (\(x,_,_,_,_,_,_) -> x) results
    startNodes = filter (endsWith 'A') $ fmap fst nodes

-- | Return a list of locations reached from the starting
-- location, terminating when we reach a valid endpoint.
-- Also return the tail of path from point at which we stop.
--
cycleToZ
  :: [(Location, (Location, Location))]
  -> Path
  -> Location
  -> [(Location, Path)]
cycleToZ nodeMap path = doit path
  where
    doit [] loc           = doit path loc
    doit (dir:dirs) loc
      | endsWith 'Z' loc  = (loc,dir:dirs) : []
      | otherwise         = case lookup loc nodeMap of
          Nothing ->
            []  -- shouldn't happen
          Just (l,r) ->
            (loc,dir:dirs) : doit dirs (case dir of L -> l ; R -> r)

endsWith :: Char -> String -> Bool
endsWith c [_,_,c3] = c == c3
endsWith _ _        = False


type Input = (Path, [Node])
type Path = [Direction]
type Node = (Location, (Location, Location))
type Location = String

data Direction = L | R
  deriving (Eq, Ord, Show)

parseInput :: Parser Input
parseInput = do
  path <- many (L <$ char 'L' <|> R <$ char 'R')
  _ <- string "\n\n"
  nodes <- parseNode `sepBy` char '\n'
  pure (path, toList nodes)
  where
    parseNode = do
      loc <- parseLocation
      _ <- string " = ("
      l <- parseLocation
      _ <- string ", "
      r <- parseLocation
      _ <- char ')'
      pure (loc, (l, r))
    parseLocation =
      (\a b c -> [a,b,c])
        <$> satisfy isAlphaNum
        <*> satisfy isAlphaNum
        <*> satisfy isAlphaNum
