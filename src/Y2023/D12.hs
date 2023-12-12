module Y2023.D12 (solutions) where

import Data.Foldable (toList)
import Data.List.NonEmpty (group)
import qualified Data.List.NonEmpty as NonEmpty

import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 = sum . fmap validArrangements

validArrangements :: (Template, [ClusterSize]) -> Int
validArrangements (template, sizes) =
  length $ filter (isValid sizes) $ fillTemplate template

fillTemplate :: Template -> [Template]
fillTemplate "" = [""]
fillTemplate (c:cs) =
  case c of
    '?' -> fmap ('#':) (fillTemplate cs) <> fmap ('.':) (fillTemplate cs)
    _   -> fmap (c:) (fillTemplate cs)

isValid :: [ClusterSize] -> Template -> Bool
isValid sizes =
  (== sizes)
  . fmap length
  . filter ((== '#') . NonEmpty.head)
  . group

type Input = [(Template,[ClusterSize])]
type Template = String
type ClusterSize = Int

parseInput :: Parser Input
parseInput = toList <$> parseLine `sepBy` char '\n'
  where
  parseLine =
    (,)
      <$> many (satisfy (/= ' ')) <* char ' '
      <*> fmap toList (int `sepBy` char ',')
