{-# LANGUAGE FlexibleInstances #-}
  
module JoinListBuffer where

import JoinList
import Scrabble
import Sized
import Buffer

singleStringToJoinList :: String -> JoinList (Score, Size) String
singleStringToJoinList s = Single (scoreString s, 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr ((+++) . singleStringToJoinList) Empty . lines
  line = indexJ
  replaceLine i newLine jl =
    takeJ i jl +++ fromString newLine +++ dropJ (i + 1) jl
  numLines = getSizeJ
  value = getScore . fst . tag

