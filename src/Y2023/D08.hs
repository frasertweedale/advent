{-# LANGUAGE ApplicativeDo #-}

module Y2023.D08 (solutions) where

import Data.Char (isAlphaNum)
import Data.Foldable (toList)

import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1]

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
