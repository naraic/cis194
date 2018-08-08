>import Prelude hiding (foldr, foldl)

foldr, foldl foldl' through example 

FOLDR

say we want to sum:

>veryBigList = [1..10000000]

>foldr f z []       = z
>foldr f z (x:xs)   = x `f` foldr f z xs

>sum1 = foldr (+) 0

>try1 = sum1 veryBigList

this should fail with a "stack overflow" exception. 
this is due to (+) being a strict function, requiring both arguements to be fully evaluated before the it can return a result:

1 + (2 + (3 + (...
pushes 1 onto the stack and becomes:
(2 + (3 + (...
which is evaluated and 2 is pushed onto the stack... and so on. 

FOLDL

the problem with the above code is that the chain of arguments cannot be reduced until the arguements have been fully expaned. i.e. it contains no reducible expressions, or REDEX for short

a redex version of this expansion might look like this:

(((0 + 1) + 2) +3)...

this alternative expansion can be achieved with an alternative to foldr - foldl. 

>foldl f z []       = z
>foldl f z (x:xs)   = let z' = z `f` x
>                     in foldl f z' xs

>sum2 = foldl (+) 0

>try2 = sum2 veryBigList

this gives the same error! 
this is because instead of being reduced, the generated sequence is put on the heap and this continues until it has been fully expanded, similarly to before. 

here, the values of intermediate statements aren't needed until the entire sum has been written with placeholder values for each step in the meantime. this time they are placed on the heap, so it eats our memory. 

then it goes back and works down the sums putting each addition onto the stack in the same way as before. 

this is due to the GHC's lazy reduction strategy:
    expressions are reduced only when they are actually needed.



FOLDL'

we need to reduce the inner redex before the outer. this is possible with seq function:

seq :: a -> b -> b

it's a primitave function that when applied to x and y will first reduce x then return y. 
it is a way to introduce strictness. 
the idea here is that y references x so that when y is retuend x has been reduced and therefore y may be computed before a large chain is built up

>foldl' f z []     = z
>foldl' f z (x:xs) = let z' = z `f` x
>                    in seq z' $ foldl' f z' xs

>sum3 = foldl' (+) 0
 
>try3 = sum3 veryBigList



CONCLUSION

usually the choice is between foldr and foldl' since foldl and foldl' are the same except for the strictness properties - if both return a result, it must be the same. 
foldl' is the moe efficient way to arrive at that result because it doesn't build up a huge thunk.
however, if the combingin function is lazy in its first argument, foldl may happily return a restult where foldl' hits an exceptiion...


there's more rules of thumb and an explaination for the above statement at the page on the haskell wiki
