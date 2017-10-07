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
