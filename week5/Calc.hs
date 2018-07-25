{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Calc where

import Parser
import ExprT
import qualified StackVM as VM

import qualified Data.Map as M
--qualified means that functions from the import _must_ be prefixed with "M." - useful!

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a 
    mul :: a -> a -> a 

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>=0)
    add = (&&)
    mul = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit n = Mod7 $ mod n 7
    add (Mod7 a) (Mod7 b) = lit $ a + b
    mul (Mod7 a) (Mod7 b) = lit $ a * b 

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval Nothing = Nothing
maybeEval (Just e) = Just $ eval e

evalStr :: String -> Maybe Integer
evalStr = maybeEval . parseExp Lit Add Mul

reify :: ExprT -> ExprT
reify = id

--ex 5

instance Expr VM.Program where
    lit n = [VM.PushI n]
    add n m = n ++ m ++ [VM.Add]
    mul n m = n ++ m ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

--ex6

data VarExprT = LitV Integer
    | AddV VarExprT VarExprT
    | MulV VarExprT VarExprT
    | Var String
    deriving (Show, Eq)

class HasVars a where
    var :: String -> a

instance Expr VarExprT where
    lit = LitV
    add = AddV
    mul = MulV

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var v = (M.lookup v)

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \ _ -> Just n -- int -> a 
    add e1 e2 = \ env -> maybeAdd (e1 env) (e2 env)
    mul e1 e2 = \ env -> maybeMul (e1 env) (e2 env)

maybeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeAdd (Just n) (Just m) = Just $ n + m
maybeAdd _ _ = Nothing

maybeMul :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeMul (Just n) (Just m) = Just $ n * m
maybeMul _ _ = Nothing


