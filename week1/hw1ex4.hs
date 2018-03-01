toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = [] 
toDigitsRev n = (mod n 10) : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEveryOtherRev (reverse l))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []     = []
doubleEveryOtherRev (x:[])   = [x]
doubleEveryOtherRev (x:(y:zs)) = x : (2*y) : (doubleEveryOtherRev zs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : ns)
    | n >= 10  = sumDigits (toDigits n) + sumDigits ns
    | otherwise = n + sumDigits ns

validate :: Integer -> Bool
validate n
         | sumDigits (doubleEveryOtherRev (toDigitsRev n)) `mod` 10 == 0 = True
         | otherwise  = False
