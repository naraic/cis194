--1

xor :: [Bool] -> Bool
xor = foldr exclusiveOr False

exclusiveOr :: Bool -> Bool -> Bool
exclusiveOr a b 
    | a && b = False
    | (not a) && (not b) = False
    | otherwise = True


--2

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x -> ((f x):)) []
