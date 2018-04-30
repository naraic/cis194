fun1 :: [Integer] -> Integer
fun1 []         = 1 
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\ x -> ((x-2)*)) 1 . filter even

fun2 :: Integer -> Integer
fun2 1               = 0
fun2 n  | even n     = n + fun2 (n `div` 2)
        | otherwise  = fun2 (n `div` 2)


fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 0) . iterate (\x -> x `div`2)
        
