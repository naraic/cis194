module Golf where

import qualified Data.Sequence as S
import Data.Foldable (toList) 
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
maxFilter (x, y, z) = y > x && y > z 

middleMap :: (Integer, Integer, Integer) -> Integer
middleMap (x, y, z) = y


--ex3
--8 lines. most disgusting code i have ever written, probably. 
-- TODO make this nice, when you've a better grasp of the language.

histogram :: [Integer] -> String
histogram xs = histHelper xs (S.replicate 10 0) 

histHelper :: [Integer] -> (S.Seq Integer) -> String 
histHelper [] n = printHist (toList n) (maximum (toList n))
histHelper (x:xs) n = histHelper xs (S.update (fromInteger x) ((S.index n (fromInteger x)) + 1) n)

printHist :: [Integer] -> Integer -> String
printHist _ 0 = "==========\n0123456789\n"
printHist s n = map (charify n) s ++ ['\n'] ++ printHist s (n - 1)

charify :: Integer -> Integer -> Char
charify m n  
    | n >= m = '*'
    | otherwise = ' '


