module Y2023.D07 (solutions) where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (groupAllWith)

import Util.Parser

solutions :: [IO ()]
solutions = [s1 parseCard go1, s1 parseCardJoker go2]

s1 :: Parser a -> (Input a -> Int) -> IO ()
s1 cardParser go =
  getContents
  >>= print . go . maybe (error "parse failed") fst . runParser (parseInput cardParser)

type Input a = [(Hand a, Bid)]
type Hand a = (a, a, a, a, a)
type Bid = Int

data Card =
  N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A
  deriving (Eq, Ord, Show)

data CardJoker =
  JJ | J2 | J3 | J4 | J5 | J6 | J7 | J8 | J9 | JT | JQ | JK | JA
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

go1 :: Input Card -> Int
go1 = sum . fmap score . zip [1..] . sortOn fst . fmap typeHand
  where
    typeHand (hand, bid) = ((handType hand, hand), bid)
    score (rank, (_, bid)) = rank * bid

handType :: (Ord a) => Hand a -> HandType
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

go2 :: Input CardJoker -> Int
go2 = sum . fmap score . zip [1..] . sortOn fst . fmap typeHand
  where
    typeHand (hand, bid) = ((handTypeJoker hand, hand), bid)
    score (rank, (_, bid)) = rank * bid

handTypeJoker :: Hand CardJoker -> HandType
handTypeJoker (a,b,c,d,e) =
  maximum $ fmap handType $ do
    a' <- replaceJoker a
    b' <- replaceJoker b
    c' <- replaceJoker c
    d' <- replaceJoker d
    e' <- replaceJoker e
    pure (a',b',c',d',e')
  where
    replaceJoker JJ = [J2, J3, J4, J5, J6, J7, J8, J9, JT, JQ, JK, JA]
    replaceJoker x  = [x]

parseInput :: Parser a -> Parser (Input a)
parseInput cardParser = fmap toList (parseLine `sepBy` char '\n')
  where
  parseLine = (,) <$> parseHand <* char ' ' <*> int
  parseHand =
    (,,,,)
      <$> cardParser
      <*> cardParser
      <*> cardParser
      <*> cardParser
      <*> cardParser

parseCard :: Parser Card
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

parseCardJoker :: Parser CardJoker
parseCardJoker =
  J2 <$ char '2'
  <|> J3 <$ char '3'
  <|> J4 <$ char '4'
  <|> J5 <$ char '5'
  <|> J6 <$ char '6'
  <|> J7 <$ char '7'
  <|> J8 <$ char '8'
  <|> J9 <$ char '9'
  <|> JT <$ char 'T'
  <|> JJ <$ char 'J'
  <|> JQ <$ char 'Q'
  <|> JK <$ char 'K'
  <|> JA <$ char 'A'
