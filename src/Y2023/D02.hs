module Y2023.D02 (solutions) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Data.Semigroup (Max(..))

import Util.Parser

solutions :: [IO ()]
solutions = [s1 gameNumberIfValid, s1 power]

type Strategy = Game -> Int

s1 :: Strategy -> IO ()
s1 strat = getContents >>= print . sum . mapMaybe (play strat) . lines

data Colour = R | G | B

data Game = Game Int (NonEmpty Round)

type Round = NonEmpty (Colour, Int)

colourCountValid :: Colour -> Int -> Bool
colourCountValid colour n = case colour of
  R -> n <= 12
  G -> n <= 13
  B -> n <= 14

roundValid :: Round -> Bool
roundValid = all (uncurry colourCountValid)

gameNumberIfValid :: Strategy
gameNumberIfValid (Game n rounds)
  | all roundValid rounds = n
  | otherwise             = 0

power :: Strategy
power (Game _ rounds) = mult (maxesGame rounds)
  where
    mult (Max r, Max g, Max b) = r * g * b
    {- Note: these would be much nicer with Foldable1 -}
    maxesGame =
      foldr (\rnd r -> maxesRound rnd <> r) (Max 0, Max 0, Max 0)
    maxesRound =
      foldr (\colourCount r -> maxesColourCount colourCount <> r) (Max 0, Max 0, Max 0)
    maxesColourCount (colour, n) =
      case colour of
        R -> (Max n, Max 0, Max 0)
        G -> (Max 0, Max n, Max 0)
        B -> (Max 0, Max 0, Max n)

-- | Parse game string and apply strategy
play :: Strategy -> String -> Maybe Int
play strat s = do
  (game, _s') <- runParser parseGame s
  pure $ strat game

parseGame :: Parser Game
parseGame = do
  Game <$ string "Game "
    <*> int <* string ": "
    <*> parseRound `sepBy` string "; "

parseRound :: Parser Round
parseRound = parseColourCount `sepBy` string ", "

parseColourCount :: Parser (Colour, Int)
parseColourCount = do
  n <- int
  _ <- char ' '
  colour <- parseColour
  pure (colour, n)

parseColour :: Parser Colour
parseColour =
  R <$ string "red"
  <|> G <$ string "green"
  <|> B <$ string "blue"
