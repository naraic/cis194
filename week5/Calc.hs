module Calc where
import ExprT
import Parser

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add 
    mul = Mul 

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr = maybeEval . (parseExp Lit Add Mul)

maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval Nothing = Nothing
maybeEval (Just e) = Just (eval e)


