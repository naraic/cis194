xor :: [Bool] -> Bool
xor = foldr exclusiveOr False

exclusiveOr :: Bool -> Bool -> Bool
exclusiveOr a b 
    | a && b = False
    | (not a) && (not b) = False
    | otherwise = True
