toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = [] 
toDigitsRev n = (mod n 10) : toDigitsRev (div n 10)

