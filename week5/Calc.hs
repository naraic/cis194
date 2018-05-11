module Calc where
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Add e1 e2) = (eval e1) + (eval e2)

evalStr :: String -> Maybe Integer
evalStr = maybeEval . (parseExp Lit Add Mul)

maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval Nothing = Nothing
maybeEval (Just e) = Just (eval e)

