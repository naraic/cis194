HIGHER-ORDER PROGRAMMING AND TYPE INFERENCE

ANONYMOUS FUNCTION

it would be convenient to not to have to give some functions names, if they will only be used once.  this frequently occurs with functions that are passed as parameters to other functions, such as with FILTER and MAP.

thus: anonymous functions...

>greaterThan100 :: [Ingeger] -> [Integer]
>greaterThan100 xs = filter (\x -> x > 100) xs

this function returns a list of Integers, all greater than 100!

the syntax for the anonymous function begins with a backslash, which is supposed to look like a lambda, with the short leg missing.

they can have multiple arguments:

>(\x y z -> [x,2*y,3*z]) 5 6 3
should return [5, 12, 9]

however - OPERATOR SECTIONS:

this can actually be written even shorter:

>greaterThan100_2 :: [Integer] -> [Integer]
>greaterThan100_2 xs = filter (>100) xs

(>100) is an operator section. if ? is an operator, then (?y) is equivalent to "\x -> x ? y"
and (y?) is equivalent to "\x -> y ? x"
here, we can partially apply an operator to one of its two arguments! we get a new function that takes one argument. SO COOL.



FUNCTION COMPOSITION

can we write a function with type:

>(b -> c) -> (a -> b) -> (a -> c) 

?

it has to take two arguments, both of which are functions. 

>foo :: (b -> c) -> (a -> b) -> (a -> c) 
>foo f g = \x f (g x)

so we apply f to (g x) using a lambda, and we are returned a "c". 

as it turns out, "foo" is actually called (.) - FUNCTION COMPOSITION 
if f and g are functions, then "f . g" is the function which first applies "g" and then applies "f".

function composition can be useful for writing concise, elegant code. 
it fits with a "wholemeal" style, where we thin about composing high-level transformations of a data structure

for example:

>myTest :: [Integer] -> Bool
>myTest xs = even (length (greaterThan100 xs))

can be rewritten as:

>myTest' :: [Integer] -> Bool
>myTest' = even . length . greaterThan100

this version makes it easier to see what's going on? or so they think. i am not convinced.
it's a "pipeline" composed of three smaller functions.
this example also demonstrates why function composition seems backwards... because application is backwards. since we read from left to right it would make more sense to think of values as flowing from left to right, but in that case we should write (x)f to denote giving the value x as an input to the function f. this is alexis claude clairaut and euler's faults

looking at the type of (.) in ghci:

>(.) :: (b -> c) -> (a -> b) -> a -> c

why no parenthesis around (a -> c)?

CURRYING AND PARTIAL APPLICATION

!!!!!!!all functions in haskell only take one argument!!!!!!!

it turns out, even functions that appear to take multiple arguments, do not.

e.g.
>f :: Int -> Int -> Int
>f x y = 2*x + y

takes one argument, and outputs a function which also takes an argument which returns the final answer

>f' :: Int -> (Int -> Int)
>f' x y = 2*x + y

note, function arrows associate to the right, so W -> X -> Y -> Z is equivalent to W -> (X -> (Y -> Z))

function application is left-associative, though. so f 3 2 is actually (f 3) 2

so

>\x y z -> ...
===
>\x -> (\y -> (\z -> ...))

and 

>f x y z = ...
===
>f = \x -> (\y -> (\z -> = ...))


therefore, we can rewrite our composition function from above by moving \x -> ... from the right hand side to the left:

>comp :: (b -> c) -> (a -> b) -> a -> c
>comp f g x = f (g x)

this idea of representing multi-argument functions as one-argument functions returning functions is known as CURRYING

if we want to actually represent a function of two arguements (dunno why we might...) we can use a single tuple argument:

>f'' :: (Int,Int) -> Int
>f'' (x,y) = 2*x + y

to convert between the two representations, CURRY and UNCURRY from prelude can be used
they might be defined like this:

schonfinkel :: ((a,b) -> c) -> a -> b -> c
schonfinkel f x y = f (x, y) 

unschonfinkel :: (a -> b -> c) -> (a,b) -> c
unschonfinkel f (x,y) = f x y 

UNCURRY is useful when you have a pair and want to applyp a function to it:

>uncurry (+) (2,3)
would give 5!


PARTIAL APPLICATION

because functions in haskell are curried, partial application is easy!
partial application means we can have a function of multiple arguments and apply it to just some of the arguments and be returned a new function 
but since there are no functions of multiple arguments, this simplifies the concept, once the function is being applied to its first argument...
the exception to this is infix operators, which we have seen can be partially applied to either of the two arguments using OPERATOR SECTION. 
in practice this turns out to be not a large limitation - there's an art to deciding the order of arguments to a function to make partial applications of it as useful as possible. 
the arguments should be ordered from "least to greatest variation" - arguments that will often be the same come first and always different come last...
so... put things like lists in the last place? 

WHOLEMEAL PROGRAMMING

here are some examples to show it:

>foobar :: [Integer] -> Integer
>foobar []       = 0
>foobar (x:xs) 
>    | x > 3     = (7*x + 2) + foobar xs
>    | otherwise = foobar xs

THIS IS BAD! too much is happening at once, and we're working too low level

instead of thinking about what we want to do to each element, we instead think about making transformations to the entire input using existing patterns.
here's a more idiomatic implementation:

>foobar' :: [Integer] -> Integer
>foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

notice map and filter hav ebeen partially applied - the type of filter is:

(a -> Bool) -> [a] -> [a]

applying it to (>3) returns a function of type [Integer] -> [Integer], which is exactly the right sort of thing to compose with another function on [Integer]

this style of coding in which we define a function without reference to its arguements - in some sense sense saying what a function _is_ rather than what it _does_, is know as a "POINT-FREE" style.
it's quite syntatically simple, but there's a limit to how much of it should be used. be weary.


FOLDS

this is a recursive pattern where the elements of a list are somehow combined:

>sum' :: [Integer] -> Integer
>sum' []         = 0
>sum' (x:xs)     = x + sum' xs

>product' :: [Integer] -> Integer
>product' []     = 1
>product' (x:xs) = x * product' xs

>length' :: [a] -> Int
>length' []      = 0
>length' (_:xs)  = 1 + length' xs

what do these functions have in common, and what are the differences? 
this might help us to abstract-out a common pattern and describe it with higher-order functions:

fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold x f (x:xs) = f x (fold z f xs)

so fold replaces [] with z and (:) with f!

fold f z [a,b,c] = a `f` (b `f` (c `f` z))

SO COOL!! and thinking about it from this perspective allows us to generalise fold onto datatypes other than lists :) 

>sum''      = fold 0 (+) 
>product''  = fold 1 (*)
>length''   = fold 0 (\_ s -> 1 + s) --so take a lits item and the running sum, and return sum + 1?

(instead of (\_ s -> 1 + s) we could also write (\_ -> (1+)) or (const (1+)) ???

fold in prelude is actually called FOLDR and the arguments are in a different order.
some prelude functions defined in terms of foldr are:

>length :: [a] -> Int
>sum :: Num a => [a] -> Int
>product :: Num => [a] -> Int
>and :: [Bool] -> Bool
>or :: [Bool] -> Bool
>any :: (a -> Bool) -> [a] -> Bool
>all :: (a -> Bool) -> [a] -> Bool

there is also FOLDL, which folds from the left:

foldr f z [a,b,c] = a `f` (b `f` (c `f` z))
foldl f z [a,b,c] = ((z `f` a) `f` b) `f` c

but in general, foldl' from Data.List should actually be used, since it's more efficient and does the same thing


