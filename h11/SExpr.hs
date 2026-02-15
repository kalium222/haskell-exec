{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isSpace)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- NOTE: 离谱
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
-- NOTE: this only parse ' ',
-- while the isSpace also parse `\t`, `\n`
-- spaces = zeroOrMore $ char ' '
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
-- NOTE: we only need the first char is Alpha
-- ident = liftA2 (++) (oneOrMore $ satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)

-- Ex. 3
atom_parser :: Parser SExpr
atom_parser = spaces *> (A . I <$> ident <|> A . N <$> posInt) <* spaces

comb_parser :: Parser SExpr
comb_parser = spaces *> char '(' *> (Comb <$> zeroOrMore s_expr_parser) <* char ')' <* spaces

s_expr_parser = atom_parser <|> comb_parser
