module AParser where

import Control.Applicative
import Data.Char

newtype Parser a
  = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-------------- Exercise 1 ---------------
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f p = Parser (\str -> first f <$> runParser p str)

-------------- Exercise 2 ---------------
instance Applicative Parser where
  pure x = Parser (\str -> Just (x, str))

  p1 <*> p2 = Parser f
    where
      f str = case (runParser p1 str) of
        Just (f', str') -> first f' <$> runParser p2 str'
        Nothing         -> Nothing

-------------- Exercise 3 ---------------
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

-------------- Exercise 4 ---------------
instance Alternative Parser where
  empty = Parser (\_ -> Nothing)

  p1 <|> p2 = Parser (\str -> runParser p1 str <|> runParser p2 str)

-------------- Exercise 5 ---------------
intOrUpperCase :: Parser ()
intOrUpperCase = ((\_ -> ()) <$> posInt) <|> ((\_ -> ()) <$> satisfy isUpper)
