module Y2023.D19 (solutions) where

import Control.Applicative
import Data.Char (isAlpha, isLower)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

import qualified Util.Map as Map
import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1, s1 go2]

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
matches part (Test cat op n) = getOp op (getCategory cat part) n

getCategory :: Category -> (a,a,a,a) -> a
getCategory cat (x,m,a,s) = case cat of
  X -> x
  M -> m
  A -> a
  S -> s

putCategory :: Category -> a -> (a,a,a,a) -> (a,a,a,a)
putCategory cat z (x,m,a,s) = case cat of
  X -> (z,m,a,s)
  M -> (x,z,a,s)
  A -> (x,m,z,s)
  S -> (x,m,a,z)

getOp :: Op -> (Int -> Int -> Bool)
getOp = \case
  GreaterThan -> (>)
  LessThan -> (<)


type Range = (Int,Int)
type Ranges = (Range,Range,Range,Range)

go2 :: Input -> Int
go2 (workflowMap, _) =
  sum
  $ fmap prodRanges
  $ filter rangesValid
  $ processWorkflow "in" startRanges
  where
    startRange = (1,4000)
    startRanges = (startRange, startRange, startRange, startRange)

    processWorkflow :: String -> Ranges -> [Ranges]
    processWorkflow k ranges = case Map.lookup k workflowMap of
      Nothing -> error $ "unknown workflow: " <> k
      Just rules -> processRules ranges rules

    processRules :: Ranges -> [Rule] -> [Ranges]
    processRules ranges = \case
      []                      -> error "rules exhausted"
      ((Nothing, r):_)        -> handleResult ranges r
      ((Just test, r):more)   ->
        let
          (yeah,nah) = splitRange test ranges
        in
          handleResult yeah r
          <> processRules nah more

    handleResult :: Ranges -> Either String Result -> [Ranges]
    handleResult ranges = \case
      Left k        -> processWorkflow k ranges
      Right Accept  -> [ranges]
      Right Reject  -> []

    prodRanges :: Ranges -> Int
    prodRanges (x,m,a,s) =
      product $ fmap (\(lo,hi) -> hi - lo + 1) [x,m,a,s]

rangesValid :: Ranges -> Bool
rangesValid (x,m,a,s) = all rangeValid [x,m,a,s]

rangeValid :: Range -> Bool
rangeValid (lo,hi) = lo <= hi

-- | Split ranges into those satisfying the test, and those not
splitRange :: Test -> Ranges -> (Ranges, Ranges)
splitRange (Test cat op n) ranges =
  let
    (lo,hi) = getCategory cat ranges
  in
    case op of
      GreaterThan ->
        ( putCategory cat (n + 1,hi) ranges   -- satisfies
        , putCategory cat (lo,n)     ranges ) -- does not satisfy
      LessThan    ->
        ( putCategory cat (lo,n - 1) ranges   -- satisfies
        , putCategory cat (n,hi)     ranges ) -- does not satisfy


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
