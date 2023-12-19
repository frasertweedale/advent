module Y2023.D18 (solutions) where

import Data.Foldable (toList)
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
  print
    -- Add half the path length (plus one).  The area computed
    -- is as from the midpoints of the paths.  However it meanders,
    -- the path does one revolution (to close the path).  So we
    -- need n path segments * 1/2m² + 4 corners * 1/4m².
    $ (+ (pathLen `div` 2 + 1))
    $ abs  -- area may be negative, take absolute value
    $ go (head path)
    $ pointPath input
  where
    pathLen = sum (fmap snd input)
    path = pointPath input
    ymax = maximum $ fmap fst path
    (ystart,xstart) = head path

    -- When moving right, add the area under the path.
    -- When moving left, subtract the area under the path.
    go (_,x1) [] =
      (xstart - x1) * (ymax - ystart)
    go (_,x1) ((y2,x2):more) =
      (x2 - x1) * (ymax - y2 - 1) + go (y2,x2) more

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
