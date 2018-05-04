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


--3

--foldr :: (a -> b -> b) -> b -> t a -> b 

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (swap f) base (reverse xs)

swap :: (a -> b -> a) -> (b -> a -> a) 
swap f x y = f y x
