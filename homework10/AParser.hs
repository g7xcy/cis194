{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

import           Data.Bifunctor (first)

import           Control.Monad  (void)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
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
-- Ex1
instance Functor Parser where
  -- p :: String -> Maybe (a, String)
  -- first f :: (a, c) -> (b, c)
  -- fmap (first f) :: Functor f => f (a, c) -> f (b, c)
  -- fmap (first f) . p :: String -> Maybe (b, c)
  fmap f (Parser p) = Parser (fmap (first f) . p)

-- Ex2
instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s)) 
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

  -- p1 :: String -> Maybe (((->)a b), String)
  -- p2 :: String -> Maybe (a, String)
  -- p1 <*> p2 = Parse p' where p' :: String -> Maybe (b, String)
  -- p1 <*> p2 succeeds iff both p1 and p2 succeed
  (<*>) (Parser p1) (Parser p2) = 
    Parser $ \s1 ->
      case p1 s1 of
        Nothing -> Nothing
        Just (f, s2) ->
          case p2 s2 of
            Nothing -> Nothing
            Just (x, s3) -> Just (f x, s3)

-- Ex3
abParser :: Parser (Char, Char)
abParser = liftA2 (,) (char 'a') (char 'b')

abParser_ :: Parser()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = liftA2 (\x y -> [x,y]) posInt (char ' ' *> posInt)

-- Ex4
instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) (Parser p1) (Parser p2) = Parser (\s -> p1 s <|> p2 s)

-- Ex5
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

