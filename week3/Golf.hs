module Golf where

--ex1
--4 lines... not great.

skips :: [a] -> [[a]]
skips l = [ skipHelper x (drop x l) | x <- [0..((length l)-1)]]

skipHelper :: Int -> [a] -> [a]  
skipHelper _ []     = []
skipHelper n (x:xs) = x : skipHelper n (drop n xs)


--ex2
--local maxima 6 lines... also not great :(

localMaxima :: [Integer] -> [Integer]
localMaxima (_:(_:[])) = []
localMaxima l = map middleMap (filter maxFilter (zip3 (drop 2 l) (init (tail l)) (take ((length l)-2) l)))

maxFilter :: (Integer, Integer, Integer) -> Bool
maxFilter (x, y, z) 
    | y > x, y > z = True
    | otherwise = False

middleMap :: (Integer, Integer, Integer) -> Integer
middleMap (x, y, z) = y
