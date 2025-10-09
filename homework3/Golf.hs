{-# OPTIONS_GHC -Wall #-}
module Golf where

import qualified Data.Map as M 
import Data.List (transpose)

-- EX1
skip :: [a] -> Int -> [a]
skip xs n = 
  let ys = drop n xs
  in case ys of
    [] -> []
    (y : ys') -> y : skip ys' n

skips :: [a] -> [[a]]
skips xs = fmap (skip xs) [0..length xs - 1]

-- EX2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [b | (a, b, c) <- zip3 xs (drop 1 xs) (drop 2 xs), b > a && b > c]

-- EX3
frequencies :: (Ord a) => [a] -> M.Map a Int
frequencies xs = M.fromListWith (+) [(x, 1) | x <- xs]

digitFrequencies :: [Int] -> [Int]
digitFrequencies xs = [M.findWithDefault 0 n fs | n <- [0..9]] where fs = frequencies xs

renderStars :: [Int] -> [String]
renderStars xs = 
  let freqs = digitFrequencies xs;
      m = maximum freqs 
  in map (\x -> replicate (m - x) ' ' ++ replicate x '*') freqs

buildHistogram :: [Int] -> String
buildHistogram = unlines . transpose . renderStars

histogram :: [Integer] -> String
histogram xs = buildHistogram ( map fromIntegral xs) ++ "==========\n0123456789\n"

