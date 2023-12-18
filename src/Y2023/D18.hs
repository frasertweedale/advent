module Y2023.D18 (solutions) where

import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)

import Util.Parser
import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 input = Map.size filled + length path
  where
    path = digPath input
    pathMap = foldr (\k -> Map.insert k ()) Map.empty path
    extents = ((ymin,xmin),(ymax,xmax))
    ymin = minimum $ fmap fst path
    ymax = maximum $ fmap fst path
    xmin = minimum $ fmap snd path
    xmax = maximum $ fmap snd path

    splay :: Location -> [Location]
    splay (y,x) = [(y - 1,x - 1),(y + 1,x + 1),(y - 1,x + 1),(y + 1,x - 1)]

    starts =
      filter
        (\k -> inBounds extents k && not (Map.member k pathMap))
        (path >>= splay)

    filled = head $ mapMaybe (floodFill pathMap extents) starts

-- | Flood fill, but abort ('Nothing') if we go outside the extents
floodFill
  :: Map.Map Location ()  -- path
  -> Extents
  -> Location             -- start
  -> Maybe (Map.Map Location ())
floodFill path extents start = go Map.empty [start]
  where
  go seen [] = Just seen
  go seen (loc:more) =
    let
      next =
        filter (\k -> not (Map.member k seen) && not (Map.member k path))
        $ steps loc
      seen' = foldr (\k -> Map.insert k ()) seen next
    in
      if not (all (inBounds extents) next)
        then Nothing
        else go seen' (more <> next)

  steps (y,x) =
    [ (y - 1, x - 1)
    , (y - 1, x    )
    , (y - 1, x + 1)
    , (y    , x - 1)
    , (y    , x + 1)
    , (y + 1, x - 1)
    , (y + 1, x    )
    , (y + 1, x + 1)
    ]

type Extents = (Location, Location) -- (lo,hi)

inBounds :: Extents -> Location -> Bool
inBounds ((ymin,xmin),(ymax,xmax)) (y,x) =
  y >= ymin && y <= ymax && x >= xmin && x <= xmax

digPath :: Input -> [Location]
digPath = go (0,0)
  where
  go _ [] = []
  go loc ((dir, n, rgb):more)
    | n <= 0 = go loc more
    | otherwise = loc : go (step loc dir) ((dir, (n - 1), rgb):more)

step :: Location -> Direction -> Location
step (y,x) = \case
  U -> (y - 1, x)
  D -> (y + 1, x)
  L -> (y, x - 1)
  R -> (y, x + 1)

type Input = [(Direction, Int, RGB)]
type RGB = (Word8, Word8, Word8)

data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

type Location = (Int,Int) -- (y,x)

parseInput :: Parser Input
parseInput = toList <$> value `sepBy` char '\n'
  where
  value =
    (,,)
    <$> direction <* char ' '
    <*> int <* string " ("
    <*> rgb <* char ')'
  rgb = (,,) <$ char '#' <*> hexByte <*> hexByte <*> hexByte
  direction =
    L <$ char 'L'
    <|> R <$ char 'R'
    <|> U <$ char 'U'
    <|> D <$ char 'D'

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
