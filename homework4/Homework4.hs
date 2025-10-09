{-# OPTIONS_GHC -Wall #-}

import Data.Set (fromList, notMember)

-- Ex1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs
  

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even 

-- Ex1
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  |  even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

collatz :: Integer -> Integer
collatz x = if odd x then 3 * x + 1 else div x 2
    
fun2' :: Integer -> Integer
fun2'  = sum . filter even . takeWhile (/= 1) . iterate collatz 

-- Ex2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node _ left v right) = 
  let leftHeight  = height left
      rightHeight = height right
  in if leftHeight <= rightHeight
     then let left' = insertTree x left 
          in Node (1 + max (height left') rightHeight) left' v right
     else let right' = insertTree x right 
          in Node (1 + max (height right') leftHeight) left v right'
      
foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- Ex3
(|^) :: Bool -> Bool -> Bool
(|^) = (/=)

xor :: [Bool] -> Bool
xor = foldr (|^) False

xor' :: [Bool] -> Bool
xor' = odd . length . filter id

-- Ex3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Ex3
-- 理解如下：
-- foldl对应
-- g(... (g (g x1 z) x2) ...) xn
-- foldr对应
-- f x1 (f x2 ... (f xn z) ...)
-- 从运算树的角度看，foldl向左偏，先应用了z和x1，然后结果再和x2应用...,foldr向右偏，先应用了xn和z，然后结果再和xn-1应用...
-- 那么我们要做的，就是使用foldr，构造一个函数链（函数组合），函数链的结果是(b -> b)的函数，最后应用到z。而函数链内部

-- 我们已经知道
-- g :: b -> a -> b
-- f :: a -> m -> m
-- 那么我们需要m == b -> b，即f :: a -> (b -> b) -> b -> b；并且我们的初始值也需要是(b -> b)的函数
-- 因此我们可以构造出函数\x acc -> acc . (...)
-- 这里，acc实际上是(b -> b)的函数
-- 根据类型，我们知道(...)的类型应该是b -> b，而(`f` x)的类型恰好满足需求。
-- 从运算的角度看，(`f` x)也构造了一个柯里化函数，把xs上从右到左的函数应用缓存起来。根据(.)的定义，最终运算结果是从左到右
-- 而初始值也需要一个(b -> b)的函数，并且不会对运算结果产生影响。因此使用id函数
-- 最后将折叠得到的函数应用到base，便是最终foldl的结果
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f base xs = foldr (\x acc -> acc . (`f` x)) id xs base

-- Ex4
innerCaculs :: Integer -> Integer -> Integer
innerCaculs x y = x + y + 2 * x * y

buildPairs :: Integer -> [(Integer, Integer)]
buildPairs n = [(x, y) | x <- [1..n], y <- [1..n], x + y + 2 * x * y <= n]

sieveSundaram :: Integer -> [Integer]
-- IMPL 1
sieveSundaram n = 
  let compositeNumber = fromList [x + y + 2 * x * y| (x, y) <- buildPairs n]
      keeps = filter (`notMember` compositeNumber) [1..n]
      primes = map ((+1) . (*2)) keeps
  in if n >= 0 then 2 : primes else primes

 -- IMPL 2
-- cartProd :: [a] -> [b] -> [(a, b)] 
-- cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- buildTable :: [Integer] -> [(Integer, Integer)] 
-- buildTable xs = [(x, y) | x <- xs, y <- xs, x <= y] 

-- sieve :: Integer -> (Integer, Integer) -> Bool 
-- sieve n (x, y) = x + y + 2 * x * y /= n 

-- sieveSundaram :: Integer -> [Integer] 
-- sieveSundaram n = 2 : map ((+1) . (*2)) (filter (\x -> foldr ((&&) . sieve x) True (buildTable xs)) xs) where xs = [1..n]
