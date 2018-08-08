--ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..] 


--ex2
fibs2 :: [Integer]
fibs2 = fibsHelper 0 1 

fibsHelper :: Integer -> Integer -> [Integer]
fibsHelper a b = a : fibsHelper b (a+b)

--ex3
data Stream a = a :< Stream a

streamToList :: Stream a -> [a]
streamToList (x:<xs) = x : streamToList xs

streamGen :: Integer -> Stream Integer
streamGen n = n :< streamGen (n+1) 

instance Show a => Show (Stream a) where
    show a = show $ take 20 $ streamToList a

--ex4
streamRepeat :: a -> Stream a
streamRepeat a = a :< streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :< xs) = f x :< streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x :< streamFromSeed f (f x) 

--ex5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :< xs) ys = x :< interleaveStreams ys xs


--(i found this online, i thought "interleaveStreams nats ruler" should work, but apparently not :'(
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler) 




