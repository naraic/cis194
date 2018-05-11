import ExprT

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Add e1 e2) = (eval e1) + (eval e2)

