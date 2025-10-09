{-# OPTIONS_GHC -Wall #-}

import Data.Ratio (numerator)

-- Ex1
fibs1 :: [Integer]
fibs1 = fib <$> [0..]
  where fib :: Integer -> Integer
        fib 0 = 0
        fib 1 = 1
        fib n = fib (n - 1) + fib (n - 2)

-- Ex2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)

fibs2' :: [Integer]
fibs2' = 0 : scanl (+) 1 fibs2'

fibs2'' :: [Integer]
fibs2'' = map (fib' fibs2'') [0..]
  where fib' _ 0 = 0
        fib' _ 1 = 1
        fib' fibs n = fibs !! fromInteger (n - 1) + fibs !! fromInteger(n - 2)

-- Ex3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = '[' : (takeStream 20 s >>= (++[]) . (++ ",") . show) ++ "...]"

instance Functor Stream where
  fmap = streamMap

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

takeStream :: Int -> Stream a -> [a]
takeStream n = take n . streamToList
 
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFilter :: (a -> Bool) -> Stream a -> Stream a
streamFilter p (Cons x xs) = if p x then Cons x ys else ys where ys = streamFilter p xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Ex5
nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x xs) ys = Cons x (interleaveStream ys xs)

ruler :: Stream Integer
ruler = interleaveStream (streamRepeat 0) (streamMap (+1) ruler)

-- Ex6
-- x' -> 0 + 1X + 0X^2 + 0X^3 + ... + 0X^n
x' :: Num a => Stream a
x' = Cons 0 (Cons 1 (streamRepeat 0))

zipWithStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithStream f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWithStream f xs ys)

scalarMulStream :: (Num a) => a -> Stream a -> Stream a
scalarMulStream x = fmap (*x)

-- Stream -> coefficients of generating functions
instance Num a => Num (Stream a) where
  fromInteger n = Cons (fromInteger n) (streamRepeat 0)
  negate = fmap negate
  (+) = zipWithStream (+)
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (scalarMulStream a0 b' + a' * b)
  abs = error "abs: not defined for power series"
  signum = error "signum: not defined for power series"

instance Fractional a => Fractional (Stream a) where
  (/) (Cons a0 a') (Cons b0 b') = q 
    where q = Cons (a0/ b0) (scalarMulStream (1 / b0) (a' - q * b'))
  fromRational r = Cons (fromRational r) (streamRepeat 0)

fibs3 :: Stream Integer
fibs3 = fmap numerator (x' / (1 - x' - x'^(2 :: Int)))

-- Ex7
data Matrix a = Matrix a a a a deriving Eq

get22 :: Matrix a -> a
get22 (Matrix _ _ _ d) = d

instance Show a => Show (Matrix a) where
  show (Matrix a b c d) = "((" ++ show a ++ ", " ++ show b ++ "), (" ++ show c ++ ", " ++ show d ++ "))"
  
instance Functor Matrix where
  fmap f (Matrix a11 a12 a21 a22) = Matrix (f a11) (f a12) (f a21) (f a22)

-- This implementation does not satisfy the mathematical definition
instance Num a => Num (Matrix a) where
  fromInteger n = Matrix x 0 0 x where x = fromInteger n
  (+) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix (a11 + b11) (a12 + b12) (a21 + b21) (a22 + b22)
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22) (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)
  negate = fmap negate
  abs = error "abs: not defined for Matrix with (*) as matrix multiply"
  signum = error "signum: not defined for Matrix with (*) as matrix multiply"

fibs4 :: Stream Integer
fibs4 = fmap fib4 (streamFromSeed (+1) 1)
  where
    fib4 :: Integer -> Integer
    fib4 = get22 . (^) (Matrix 1 1 1 0)

