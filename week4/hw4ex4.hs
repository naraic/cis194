--finding primes using the sieve of sundaram
--(using function composition)

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\ x -> (2*x)+1) . sieveHelper . (enumFromTo 1) 

sieveHelper :: [Integer] -> [Integer] 
sieveHelper xs = xs \\ (map (\ (x,y) -> x+y+(2*x*y)) (cartProdDoubler xs))

cartProdDoubler :: [a] -> [(a, a)]
cartProdDoubler xs = cartProd xs xs

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

