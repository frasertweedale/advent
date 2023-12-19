module Y2023.D18 (solutions) where

import Data.Foldable (toList)
import Data.List (sort)
import Data.List.NonEmpty (groupWith)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Word (Word8)

import Util.Parser
import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [s1, s2, draw]

s1 :: IO ()
s1 =
  getContents
  >>= go2 . maybe (error "parse failed") fst . runParser parseInput

s2 :: IO ()
s2 =
  getContents
  >>= go2 . maybe (error "parse failed") fst . runParser parseInput2

draw :: IO ()
draw =
  getContents
  >>= putStrLn . drawPath
      . maybe (error "parse failed") fst . runParser parseInput

go2 :: Input -> IO ()
go2 input =
  print $ sum $ fmap (\(dy, spans, extra) -> dy * gaps spans + extra)
  $ go 0 []
  $ fmap (\row -> (fst (NonEmpty.head row), toList (fmap snd row)))
  $ groupWith fst
  $ sort
  $ pointPath input
  where
  go _ _ [] = []
  go y spans ((y', xs):rows) =
    let
      joined = joinSpans spans xs
      extraPath = sum $ fmap fst joined
      spans' = fmap snd joined
      -- Filter out the dummy (0,0) spans (these are paired with
      -- horizontal path segments wider than the resulting span).
      filteredSpans' = filter (/= (0,0)) spans'
      fusedSpans' = fuseSpans filteredSpans'
      -- For each join there is an "overlap" of 1, which
      -- needs to be subtracted.
      joins = length filteredSpans' - length fusedSpans'
    in
      ((y' - y), spans, extraPath - joins) : go y' fusedSpans' rows

  fuseSpans :: [Span] -> [Span]
  fuseSpans = \case
    []                                      -> []
    ((lo1,hi1):(lo2,hi2):more) | hi1 == lo2 -> fuseSpans ((lo1,hi2):more)
    ((x1,x2):more)                          -> (x1,x2) : fuseSpans more

  gaps :: [Span] -> Int
  gaps = sum . fmap (\(x1,x2) -> x2 - x1 + 1)

drawPath :: Input -> String
drawPath input =
  show extents <> "\n"
  <> unlines (fmap drawLine [ymin..ymax])
  where
  drawLine :: Int -> String
  drawLine y = do
    x <- [xmin..xmax]
    maybe " " (const "#") $ Map.lookup (y,x) pathMap
  path = digPath input
  ymin = minimum $ fmap fst path
  ymax = maximum $ fmap fst path
  xmin = minimum $ fmap snd path
  xmax = maximum $ fmap snd path
  extents = ((ymin,xmin),(ymax,xmax))
  pathMap = foldr (\k -> Map.insert k ()) Map.empty path


type Span = (Int, Int) -- (lo,hi)

joinSpans :: [Span] -> [Int] -> [(Int, Span)]
joinSpans [] (lo:hi:xs)  = (hi - lo + 1, (lo,hi)) : joinSpans [] xs
joinSpans spans []       = fmap (0,) spans
joinSpans _     [_]      = error "can't happen"

joinSpans ((lo1,hi1):(lo2,hi2):more) (x1:x2:xs)
  --  |##|___|##...   there could be multiple spans fusing, so we have to
  --  |#########...   push the expanded span back onto the span list
  | x1 == hi1, x2 == lo2 = (x2 - x1 - 1, (0,0)) : joinSpans ((lo1,hi2):more) xs
                                      -- ^^^^^ fake it, filter later

joinSpans spans@((lo,hi):more) (x1:x2:xs)
  --  ____  |##|
  --  |##|  |##|
  | x2 < lo             = (x2 - x1 + 1, (x1,x2))  : joinSpans spans xs

  --  |##|  ____
  --  |##|  |##|
  | x1 > hi             = (0,           (lo,hi))  : joinSpans more (x1:x2:xs)

  --  |##|
  --   ▔▔
  | (x1,x2) == (lo,hi)  =                           joinSpans more xs

  --  |##|______
  --  |########|
  | x1 == hi            = (x2 - hi,     (lo,x2))  : joinSpans more xs

  --  ______|##...
  --  |########...
  | x2 == lo            = (lo - x1,       (0,0))  : joinSpans ((x1,hi):more) xs

  --  |########...
  --  ▔▔▔▔▔▔|##...
  | x1 == lo            = (x1 - lo,       (0,0))  : joinSpans ((x2,hi):more) xs

  --  |########|
  --  |##|▔▔▔▔▔▔
  | x2 == hi            = (hi - x2,     (lo,x1))  : joinSpans more xs

  --  |#########...
  --  |##|▔▔▔|##...
  | x2 < hi             = (0,           (lo,x1))  : joinSpans ((x2,hi):more) xs

  | otherwise           = error $ "bad: " <> show (x1,x2) <> show (lo,hi)

digPath :: Input -> [Location]
digPath = go (0,0)
  where
  go _ [] = []
  go loc ((dir, n):more)
    | n <= 0 = go loc more
    | otherwise = loc : go (step loc dir) ((dir, (n - 1)):more)

step :: Location -> Direction -> Location
step (y,x) = \case
  U -> (y - 1, x)
  D -> (y + 1, x)
  L -> (y, x - 1)
  R -> (y, x + 1)

-- | A list of points that are the corners of the path.
pointPath :: Input -> [Location]
pointPath = go (0,0)
  where
  go _ [] = []
  go loc@(y,x) ((dir, n):more) =
    let
      loc' = case dir of
        U -> (y - n, x    )
        D -> (y + n, x    )
        L -> (y,     x - n)
        R -> (y,     x + n)
    in
      loc : go loc' more

type Input = [(Direction, Int)]

data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

type Location = (Int,Int) -- (y,x)

parseInput :: Parser Input
parseInput = toList <$> value `sepBy` char '\n'
  where
  value =
    (,)
    <$> direction <* char ' '
    <*> int <* string " ("
    <* rgb <* char ')'
  rgb = (,,) <$ char '#' <*> hexByte <*> hexByte <*> hexByte
  direction =
    L <$ char 'L'
    <|> R <$ char 'R'
    <|> U <$ char 'U'
    <|> D <$ char 'D'

parseInput2 :: Parser Input
parseInput2 = toList <$> value `sepBy` char '\n'
  where
  value =
    flip (,)
    <$  satisfy (const True) <* char ' '
    <*  int <* string " (#"
    <*> ( foldl1 (\b a -> b * 16 + a) . fmap fromIntegral
        <$> sequenceA (replicate 5 hexNibble) )
    <*> direction <* char ')'
  direction =
    R <$ char '0'
    <|> D <$ char '1'
    <|> L <$ char '2'
    <|> U <$ char '3'

hexByte :: Parser Word8
hexByte = do
  hi <- hexNibble
  lo <- hexNibble
  pure $ hi * 16 + lo

hexNibble :: Parser Word8
hexNibble =
  0 <$ char '0'
  <|> 1 <$ char '1'
  <|> 2 <$ char '2'
  <|> 3 <$ char '3'
  <|> 4 <$ char '4'
  <|> 5 <$ char '5'
  <|> 6 <$ char '6'
  <|> 7 <$ char '7'
  <|> 8 <$ char '8'
  <|> 9 <$ char '9'
  <|> 10 <$ char 'a'
  <|> 11 <$ char 'b'
  <|> 12 <$ char 'c'
  <|> 13 <$ char 'd'
  <|> 14 <$ char 'e'
  <|> 15 <$ char 'f'
