{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char (toUpper)

-- Ex3
newtype Score = Score Int deriving (Eq, Ord, Show)

instance Num Score where
  fromInteger = Score . fromInteger
  (+) (Score x) (Score y) = Score (x + y)
  (*) (Score x) (Score y) = Score (x * y)
  abs (Score x) = Score (abs x)
  signum (Score x) = Score (signum x)
  negate (Score x) = Score (negate x)
    
  
instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

getScore :: Score -> Int
getScore (Score x) = x

score :: Char -> Score
score c = case toUpper c of
  'A' -> 1; 'E' -> 1; 'I' -> 1; 'O' -> 1; 'U' -> 1
  'L' -> 1; 'N' -> 1; 'S' -> 1; 'T' -> 1; 'R' -> 1
  'D' -> 2; 'G' -> 2
  'B' -> 3; 'C' -> 3; 'M' -> 3; 'P' -> 3
  'F' -> 4; 'H' -> 4; 'V' -> 4; 'W' -> 4; 'Y' -> 4
  'K' -> 5
  'J' -> 8; 'X' -> 8
  'Q' -> 10; 'Z' -> 10
  _ -> 0

scoreString :: String -> Score
scoreString = foldr ((+) . score) 0

