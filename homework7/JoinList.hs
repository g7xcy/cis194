{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Scrabble
import Sized

-- m is an instance of monoid
-- the combination opreation is based on the implementation of m
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

instance Sized b => Sized (JoinList b a) where
  size Empty = 0
  size (Single s _) = size s
  size (Append s _ _) = size s

-- Ex1
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = error "tag: no monoid is annotated with Empty"

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x Empty = x
(+++) Empty x = x
(+++) x y = Append (tag x <> tag y) x y

-- Ex2
getSizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
getSizeJ Empty = 0
getSizeJ jl = getSize . size $ jl

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i x | i >= getSizeJ x  = Nothing
indexJ 0 (Single _ v) = Just v
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ l r)
  | i < leftSize = indexJ i l
  | otherwise = indexJ (i - leftSize) r
  where leftSize = getSizeJ l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl 
  | n <= 0 = jl
  | n >= getSizeJ jl = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r)
  | n < leftSize = dropJ n l +++ r
  | n == leftSize = r
  | otherwise = dropJ (n - leftSize) r
  where leftSize = getSizeJ l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl 
  | n <= 0 = Empty
  | n >= getSizeJ jl = jl
takeJ _ jl@(Single _ _) = jl
takeJ n (Append _ l r)
  | n < leftSize = takeJ n l
  | otherwise = l +++ takeJ (n - leftSize) r 
  where leftSize = getSizeJ l

-- Ex3
scoreLine :: String -> JoinList Score String
scoreLine s =  
  case scoreString s of
    0 -> Empty
    n -> Single n s

-- Ex4
getValue :: Monoid b => JoinList a b -> b
getValue Empty = mempty
getValue (Single _ v) = v
getValue (Append _ l r) = getValue l <> getValue r

-- instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
--   (<>) (a1, b1) (a2, b2) = (a1 <> a2, b1 <> b2)
  
-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)
  
-- Helpful Functions
(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) _ i | i < 0 = Nothing
(!!?) (x:xs) i
  | i == 0 = Just x
  | otherwise = (!!?) xs (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ x) = [x]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

