{-# OPTIONS_GHC -Wall #-}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n from to via
  | n <= 0 = []
  | otherwise =  hanoi (n - 1) from via to ++ [(from, to)] ++ hanoi (n - 1) via to from

