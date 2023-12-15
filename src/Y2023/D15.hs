module Y2023.D15 (solutions) where

import Data.Char (isAlpha, ord)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Data.List.NonEmpty (toList)

import Util.Parser
import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [s1, s2]

s1 :: IO ()
s1 =
  getContents
  >>= print . go
    . maybe (error "parse failed") fst . runParser parseInput . filter (/= '\n')
  where
    go :: Input -> Int
    go = sum . fmap (fromIntegral . hash)

type BoxMap = Map.Map Word8 [(Label, FocalLength)]

s2 :: IO ()
s2 =
  getContents
  >>= print
    . focusingPower
    . go
    . maybe (error "parse failed") fst . runParser parseInput2 . filter (/= '\n')
  where
    go :: Input2 -> BoxMap
    go = foldl step Map.empty

    step :: BoxMap -> (Label, Operation) -> BoxMap
    step m (label, instr) = case instr of
      RemoveLens      -> Map.insertWith (rm label) (hash label) [] m
      AddLens focLen  -> Map.insertWith add (hash label) [(label, focLen)] m

    rm label new cur = case cur of
      []                      -> []
      (h:t) | fst h == label  -> t
            | otherwise       -> h : rm label new t

    add new []          = new
    add new@(a:_) (h:t)
      | fst a == fst h  = a : t
      | otherwise       = h : add new t
    add _ _old          = error "can't happen?"

    focusingPower :: Map.Map Word8 [(Label, FocalLength)] -> Int
    focusingPower m = sum $ fmap (\(i,box) -> i * boxPower box) numberedBoxes
      where
        boxes :: [ [(Label, FocalLength)] ]
        boxes = fmap (\k -> fromMaybe [] $ Map.lookup k m) [0..255]

        numberedBoxes = zip [1..] boxes

        boxPower [] = 0
        boxPower xs = sum $ fmap (uncurry (*)) $ zip [1..] $ fmap snd xs


hash :: String -> Word8
hash = foldl step 0
  where
  step acc c = (acc + fromIntegral (ord c)) * 17

type Input = [String]

type Input2 = [(Label, Operation)]
data Operation = AddLens FocalLength | RemoveLens
type Label = String
type FocalLength = Int

parseInput :: Parser Input
parseInput = toList <$> many (satisfy (/= ',')) `sepBy` char ','

parseInput2 :: Parser Input2
parseInput2 = toList <$> labelAndOperation `sepBy` char ','
  where
    labelAndOperation =
      (,)
      <$> many (satisfy isAlpha)
      <*> operation
    operation =
      RemoveLens <$ char '-'
      <|> AddLens <$ char '=' <*> int
