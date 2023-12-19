module Y2023.D19 (solutions) where

import Control.Applicative
import Data.Char (isAlpha, isLower)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

import qualified Util.Map as Map
import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 (workflowMap, parts) =
  sum $ fmap sumPart $ mapMaybe (process "in") parts
  where
    process k part = case Map.lookup k workflowMap of
      Nothing -> error $ "unknown workflow: " <> k
      Just rules -> case matchRule part rules of
        Left k' -> process k' part
        Right Accept -> Just part  -- keep
        Right Reject -> Nothing    -- discard

    sumPart (x,m,a,s) = x + m + a + s

matchRule :: Part -> [Rule] -> Either String Result
matchRule part = \case
  []                      -> error "rules exhausted"
  ((Nothing, r):_)        -> r
  ((Just test, r):more)
    | part `matches` test -> r
    | otherwise           -> matchRule part more

matches :: Part -> Test -> Bool
matches part (Test cat op n) = getOp op (getCategory part cat) n

getCategory :: Part -> Category -> Int
getCategory (x,m,a,s) = \case
  X -> x
  M -> m
  A -> a
  S -> s

getOp :: Op -> (Int -> Int -> Bool)
getOp = \case
  GreaterThan -> (>)
  LessThan -> (<)


type Input = (Map.Map String [Rule], [Part])

type Part = (Int, Int, Int, Int) -- x, m, a, s

data Category = X | M | A | S
  deriving (Eq, Ord, Show)

data Result = Reject | Accept
  deriving (Eq, Ord, Show)

type Rule = (Maybe Test, Either String Result)

data Test = Test Category Op Int
  deriving (Eq, Ord, Show)

data Op = GreaterThan | LessThan
  deriving (Eq, Ord, Show)

parseInput :: Parser Input
parseInput = (,) <$> workflows <* string "\n\n" <*> parts
  where
  workflows =
    foldr (uncurry Map.insert) Map.empty. toList
    <$> workflow `sepBy` char '\n'
  parts = toList <$> part `sepBy` char '\n'
  rules = toList <$> rule `sepBy` char ','

  name = some (satisfy (\c -> isAlpha c && isLower c))

  workflow =
    (,)
      <$> name
      <* char '{' <*> rules <* char '}'

  rule =
    (,)
      <$> optional (test <* char ':')
      <*> (Left <$> name <|> Right <$> result)

  test = Test <$> category <*> op <*> int

  category =
    X <$ char 'x'
    <|> M <$ char 'm'
    <|> A <$ char 'a'
    <|> S <$ char 's'

  op =
    GreaterThan <$ char '>'
    <|> LessThan <$ char '<'

  result =
    Accept <$ char 'A'
    <|> Reject <$ char 'R'

  part =
    (,,,)
      <$ string "{x=" <*> int
      <* string ",m=" <*> int
      <* string ",a=" <*> int
      <* string ",s=" <*> int <* char '}'
