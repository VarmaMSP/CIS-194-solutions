module SExpr where

import AParser

import Data.Char
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p $ zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

atom :: Parser Atom
atom = (N <$> posInt) <|> (I <$> ident)

ignoreSpaces :: Parser a -> Parser a
ignoreSpaces p = spaces *> p <* spaces

ignoreBraces :: Parser a -> Parser a
ignoreBraces p = char '(' *> p <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = ignoreSpaces $
             (A <$> atom) <|> (ignoreBraces $ Comb <$> oneOrMore parseSExpr)
