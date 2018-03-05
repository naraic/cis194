toDigits :: Integer -> [Integer]
toDigits n  = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherLeft (reverse n))

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft []         = []
doubleEveryOtherLeft (n:[])       = n:[]
doubleEveryOtherLeft (n:(m:ns))     = n:m*2:doubleEveryOtherLeft ns

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n:ns) = sum (toDigitsRev n) + sumDigits ns

validate :: Integer -> Bool
validate n = (sumDigits(doubleEveryOtherLeft (toDigitsRev n))) `mod` 10 == 0
