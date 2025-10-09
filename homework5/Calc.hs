{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

import qualified ExprT as E
import Parser (parseExp)
import qualified StackVM
import qualified Data.Map as M

-- Ex1
eval :: E.ExprT -> Integer
eval (E.Lit i) = i
eval (E.Add exprT1 exprT2) = eval exprT1 + eval exprT2
eval (E.Mul exprT1 exprT2) = eval exprT1 * eval exprT2

-- Ex2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- Ex3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

-- Ex4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y )= MinMax (min x y)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

-- Ex5
instance Expr StackVM.Program where
  lit = (:[]) . StackVM.PushI
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

-- Ex6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
     deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var
  
instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul
  
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add x y m = liftA2 (+) (x m) (y m)
  mul x y m = liftA2 (*) (x m) (y m) 

