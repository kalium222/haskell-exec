module AParser where

import Control.Applicative
import Data.Char
import Data.Functor

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

-- Ex.1
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  -- NOTE: init version:
  -- in this version, we handled Maybe by hand
  -- fmap a2b (Parser runParser_a) = Parser runParser_b
  --   where
  --     runParser_b str = case runParser_a str of
  --       Nothing -> Nothing
  --       Just (a, str') -> Just (a2b a, str')
  fmap a2b (Parser runParser_a) = Parser runParser_b
    where
      runParser_b str = first a2b <$> (runParser_a str)

-- NOTE: or,
-- fmap a2b (Parser run_a) = Parser (fmap (first a2b) . run_a)

-- Ex.2
instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))

  -- NOTE: init version:
  -- in this version, we handled Maybe by hand
  -- but we can see that `Just (a2b a, str'')` very clearly
  -- (Parser runParser_a2b) <*> (Parser runParser_a) = Parser idk_whatisit
  --   where
  --     idk_whatisit str =
  --       case runParser_a2b str of
  --         Nothing -> Nothing
  --         Just (a2b, str') -> case runParser_a str' of
  --           Nothing -> Nothing
  --           Just (a, str'') -> Just (a2b a, str'')

  (Parser runParser_a2b) <*> parser_a = Parser $ \str ->
    case runParser_a2b str of
      Nothing -> Nothing
      Just (a2b, str') -> runParser (fmap a2b parser_a) str'

{--
    NOTE: Example:
    data Employee = Emp {name::Name, phone::String}
    if we have
    parseName :: Parser Name
    parsePhone :: Parser Phone
    then we can have
    Emp <$> parseName <*> parserPhone :: Parser Employee

    HACK:
    because Emp :: Name -> String -> Employee
    Emp <$> parseName :: (Name -> String -> Employee) <$> Parser Name
    :: Parser (String -> Employee)
    here we can check what it is:
    Emp <$> parseName
    = fmap Emp (Parser runParser_a) = Parser runParser_b
        where
          runParser_b str = case runParser_a str of
            Nothing -> Nothing
            Just (a, str') -> Just (Emp a, str')

    HACK:
    and after this, where `<*> parserPhone`, will apply `Emp a`
    to remaining phone, in line 26
--}

-- Ex.3
-- NOTE: find out about *>, <*, $>, <$, <|>
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> (char 'a') <*> (char 'b')

-- NOTE:
-- f a *> f b -> f b
-- work as `f b`, but keep the logic of `f` of `f a`
-- <* is similar as it
abParser_' :: Parser ()
abParser_' = char 'a' *> char 'b' *> pure ()

-- NOTE:
-- a1 <$ f a2 -> f a1
-- keep the logif of `f` of `f a2`
-- then `fmap (const a1) (f a2) => f a1`
-- see definition of <$
abParser_'' :: Parser ()
abParser_'' = () <$ char 'a' <* char 'b'

-- Or use $> in Data.Functor
abParser_''' :: Parser ()
abParser_''' = char 'a' *> char 'b' $> ()

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> (char ' ') <*> posInt

intPair' :: Parser [Integer]
intPair' = (\x y -> [x, y]) <$> posInt <* (char ' ') <*> posInt

-- NOTE: need to know how sequenceA of [] is defined
intPair'' :: Parser [Integer]
intPair'' = sequenceA [posInt, char ' ' *> posInt]

-- Ex.4
instance Alternative Parser where
  empty = Parser (\_ -> Nothing)

  -- NOTE: long
  -- (Parser runParser_a) <|> (Parser runParser_b) = Parser f
  --   where
  --     f str = case runParser_a str of
  --       res@(Just _) -> res
  --       Nothing -> runParser_b str
  Parser p1 <|> Parser p2 = Parser (\str -> p1 str <|> p2 str)

-- Ex. 5
intOrUppercase :: Parser ()
intOrUppercase = (posInt $> ()) <|> (satisfy isUpper $> ())

-- WARNING: I dont fucking know what shit it is
--
-- WARNING: I fucking understand now!

-- Functor:
-- (<$)        :: a -> f b -> f a
-- (<$)        =  fmap . const
--
-- Applicative:
-- (*>) :: f a -> f b -> f b
-- a1 *> a2 = (id <$ a1) <*> a2
-- (<*) :: f a -> f b -> f a
-- (<*) = liftA2 const
