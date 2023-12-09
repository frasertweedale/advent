{-# LANGUAGE ApplicativeDo #-}

module Util.Parser
  ( Parser(..)
  , endOfInput
  , satisfy
  , char
  , spaces
  , string
  , int
  , sepBy
  , module Control.Applicative
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (a, s') <- runParser p s
    pure $ (f a, s')

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  pf <*> pa = Parser $ \s -> do
    (f, s') <- runParser pf s
    (a, s'') <- runParser pa s'
    Just (f a, s'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

endOfInput :: Parser ()
endOfInput = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy test = Parser $ \s -> case s of
  c:cs | test c -> Just (c, cs)
  _             -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

int :: Parser Int
int = do
  (negate <$ char '-' <|> pure id)
  <*> (read <$> digits)
  where
  digits = (:) <$> satisfy isDigit <*> many (satisfy isDigit)

-- | Zero or more spaces
spaces :: Parser [Char]
spaces = many (char ' ')


string :: String -> Parser String
string match = Parser $ \s ->
  if match `isPrefixOf` s
    then Just (match, drop (length match) s)
    else Nothing

sepBy :: Parser a -> Parser sep -> Parser (NonEmpty a)
sepBy p sep = (:|) <$> p <*> many (sep *> p)
