{-# LANGUAGE ApplicativeDo #-}

module Y2023.D02 (solutions) where

import Data.Maybe (mapMaybe)

import Util.Parser

solutions :: [IO ()]
solutions = [s1]

s1 :: IO ()
s1 = getContents >>= print . sum . mapMaybe play . lines

data Colour = R | G | B

data Game = Game Int [Round]

type Round = [(Colour, Int)]

colourCountValid :: Colour -> Int -> Bool
colourCountValid colour n = case colour of
  R -> n <= 12
  G -> n <= 13
  B -> n <= 14

roundValid :: Round -> Bool
roundValid = all (uncurry colourCountValid)

-- | Parse game string and return game number if game is valid
play :: String -> Maybe Int
play s = do
  (Game n rounds, _s') <- runParser parseGame s
  if all roundValid rounds
    then Just n
    else Nothing

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
