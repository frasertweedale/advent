module Y2023.D07 (solutions) where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (groupAllWith)

import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1]

s1 :: (Input -> Int) -> IO ()
s1 go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser parseInput

type Input = [(Hand, Bid)]
type Hand = (Card, Card, Card, Card, Card)
type Bid = Int

data Card =
  N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A
  deriving (Eq, Ord, Show)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

go1 :: Input -> Int
go1 = sum . fmap score . zip [1..] . sortOn fst . fmap typeHand
  where
    typeHand (hand, bid) = ((handType hand, hand), bid)
    score (rank, (_, bid)) = rank * bid

handType :: Hand -> HandType
handType (a,b,c,d,e) = check $ fmap length $ groupAllWith id [a,b,c,d,e]
  where
    check lengths
      | any (== 5) lengths = FiveOfAKind
      | any (== 4) lengths = FourOfAKind
      | any (== 3) lengths && any (== 2) lengths = FullHouse
      | any (== 3) lengths = ThreeOfAKind
      | length (filter (== 2) lengths) == 2 = TwoPair
      | length (filter (== 2) lengths) == 1 = OnePair
      | otherwise = HighCard

parseInput :: Parser Input
parseInput = fmap toList (parseLine `sepBy` char '\n')
  where
  parseLine = (,) <$> parseHand <* char ' ' <*> int
  parseHand =
    (,,,,)
      <$> parseCard
      <*> parseCard
      <*> parseCard
      <*> parseCard
      <*> parseCard
  parseCard =
    N2 <$ char '2'
    <|> N3 <$ char '3'
    <|> N4 <$ char '4'
    <|> N5 <$ char '5'
    <|> N6 <$ char '6'
    <|> N7 <$ char '7'
    <|> N8 <$ char '8'
    <|> N9 <$ char '9'
    <|> T <$ char 'T'
    <|> J <$ char 'J'
    <|> Q <$ char 'Q'
    <|> K <$ char 'K'
    <|> A <$ char 'A'
