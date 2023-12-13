{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Y2023.D12 (solutions) where

import Data.Foldable
import Data.List (intercalate)
import Data.List.NonEmpty (groupWith)
import qualified Data.List.NonEmpty as NonEmpty

import Util.Parser

solutions :: [IO ()]
solutions = [s1 (go1 id), s1 (go1 prep2)]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

go1 :: ((Template,[ClusterSize]) -> (Template,[ClusterSize])) -> Input -> Int
go1 prep = sum . fmap (uncurry walk . prep)

prep2 :: (Template,[ClusterSize]) -> (Template,[ClusterSize])
prep2 (template, sizes) =
  ( intercalate "?" $ replicate 5 template
  , concat $ replicate 5 sizes
  )

data PathState
  -- | Need to consume a '#', or a '?' which we will treat as a '#'
  = NeedHash Int
  -- | Need to consume a '.', or a '?' which we will treat as a '.'
  -- (group just ended, need a gap).
  | NeedDot
  -- | Can eat any character.  '#' will start eating a new hash
  -- group, '.' will just carry on at the next character, and
  -- '?' will produce both next states.
  | Don'tCare
  deriving (Eq, Ord, Show)

data Path = Path PathState [ClusterSize]
  deriving (Eq, Ord, Show)

feedPath :: Char -> Path -> [Path]
feedPath c = \case
  Path (NeedHash n) xs -> case c of
    '.'             -> []
    _ | n > 1       -> [ Path (NeedHash (n - 1)) xs ]
    _               -> [ Path NeedDot xs ]
  Path NeedDot xs -> case c of
    '#'             -> []
    _               -> [ Path Don'tCare xs ]
  Path Don'tCare [] -> case c of
    '#'             -> []
    _               -> [ Path Don'tCare [] ]
  Path Don'tCare (n:xs) -> case c of
    '#' | n > 1     -> [ Path (NeedHash (n - 1)) xs ]
        | otherwise -> [ Path NeedDot xs ]
    '?' | n > 1     -> [ Path Don'tCare (n:xs)
                       , Path (NeedHash (n - 1)) xs ]
        | otherwise -> [ Path Don'tCare (n:xs)
                       , Path NeedDot xs ]
    _ {- '.' -}     -> [ Path Don'tCare (n:xs) ]

walk :: Template -> [ClusterSize] -> Int
walk s xs = go s (start xs)
  where
    go "" paths =
      sum . fmap (\(mul, path) -> mul * endPath path) $ paths
    go (c:cs) paths =
      go cs (step c paths)

step :: Char -> [(Int, Path)] -> [(Int, Path)]
step c paths =
  joinPaths
  $ paths >>= \(mul, path) -> fmap (mul,) (feedPath c path)

-- | Gather distinct paths, taking the sum of their multiplicands.
--
joinPaths :: [(Int, Path)] -> [(Int, Path)]
joinPaths =
  fmap (\l -> (sum (fmap fst l), snd (NonEmpty.head l)))
  . groupWith snd

-- | The pattern has terminated.  Express validity of
-- the residual Path value as a multiplicand (0 or 1).
--
endPath :: Path -> Int
endPath = \case
  Path NeedDot []   -> 1
  Path Don'tCare [] -> 1
  _                 -> 0

-- Construct start state
start :: [ClusterSize] -> [(Int, Path)]
start xs = [ ( 1, Path Don'tCare xs ) ]


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
