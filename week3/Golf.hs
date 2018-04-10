module Golf where

skips :: [a] -> [[a]]
skips l = [ skipHelper x (drop x l) | x <- [0..((length l)-1)]]

skipHelper :: Int -> [a] -> [a]  
skipHelper _ []     = []
skipHelper n (x:xs) = x : skipHelper n (drop n xs)
