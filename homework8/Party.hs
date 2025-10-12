{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Data.List (sort)
import Data.Tree
import Employee

-- Ex1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL gs fun) = GL (emp : gs) (empFun emp + fun)

instance Semigroup GuestList where
  (<>) (GL g1 fun1) (GL g2 fun2) = GL (g1 ++ g2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Ex2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v ts) = f v (fmap (treeFold f) ts)

-- Ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss kids = (withBoss, withoutBoss)
  where
    withoutBoss = foldMap (uncurry max) kids
    withBoss = glCons boss (foldMap snd kids)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Ex4
formatGuests :: GuestList -> String
formatGuests (GL xs fun) = 
  "Total fun: " ++ 
  show fun ++
  "\n" ++
  unlines (sort (fmap empName xs))

main :: IO ()
main = readFile "company.txt" >>= putStrLn . formatGuests . maxFun . read

