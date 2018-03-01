
haskell is FUNCTIONAL 

by which we mean that 
	1 functions are first-class: they can be used the same way as other values
	2 program meanings centered around evaluating expressions rather than executing instructions

haskell is PURE
	1 no mutation - variables and data structures are immutable
	2 expressions never have side effects
	3 functions results are the same if the inputs are the same

      these things are great because
      	1 it can make refactoring more simple when replacing things
      	2 concurrent programs are easier since expressions don't affect each other
      	3 generally less headaches (this is legit)

haskell is LAZY
	-expressions aren't evaluated until their results are needed
	
	1 it is easy to define a new control struction by defining a function ?
	2 infinite data structures are possible
	3 enables compositional programming style
	4 (con) reasoning about time and space is harder :(

haskell is STATICALLY TYPED
	each expression has a type and these are checked at compile time to point out bugs before running


COURSE THEMES

 	TYPES
 	- in haskell can help clarify thinking and express program structure
 	- serves as documentation
 	- turns run-time errors into compile-time errors

 	ABSTRACTION
	- "dont't repeat yourself" 	
	- helped here by parametric polymorphism, higher-order functions & type classes

	WHOLEMEAL PROGRAMMING
	-think big! solve the general problem first, look at issues from a high level of abstractoin


the code:


DECLARATIONS AND VARIABLES:

>x :: Int
>x = 3

:: means "has type"
=  means "is defined to be" and _not_ "gets" or "is assigned"

so... x has type Int and x is defined to be three - this cannot be changed (immutable)


BASIC TYPES:

* Int - machine sized integers, _at least_ 29bit 2s complement. wut

try:
>biggestInt, smallestInt :: Int
>biggestInt = maxBound
>smallestInt = minBound

note: idiomatic haskell uses (lower) camelCase

* Integer - arbitrary-precision integers

* Double - double precision floating point

* Single - single precision floating point

* Bool - True or False!?

* Char - unicode characters

* String - lists of characters


GHCi:

the interpreter - REPL
load files with :load (:l)
reload files with :reload (:r)
get expression type with :type (:t)
get help with :?


Arithmetic:

fairly standard... + - * / mod ^ (-x) div
mod is a prefix (mod 7 3) function so use backticks to make it infix (7 `mod` 3)
negative numbers need parentheses, probably

haskell does not do implicit conversion between types, so you must cast:
	- fromIntegral (Int or Integer to any other numberic type)
	- round, floor, ceiling (floating point numbers to Int or Integer)

/ is for floating-point division only
div is for integer division


Boolean Logic: 

&& || not

Comparison:

== /= < > <= >=

* If-expressions:
	if b then t else f 
	the else expression is not optional, but this isn't used much because of pattern-matching (guards)


Defining Basic Functions: 

with cases:

>sumtorial :: Integer -> Integer
>sumtorial 0 = 0 
>sumtorial n = n + sumtorial (n - 1)

first line - sumtorial takes an Integer and returns an Integer
clauses are checked from top to bottom and the first matching clause is chosen

>hailstone :: Integer -> Integer
>hailstone n
>    | n `mod` 2 == 0 = n `div` 2
>    | otherwise      = 3*n + 1

the above example uses guards, they must start with a boolean
otherwise is always true
you can use any number of guards

here is a complex contrived example: 

>foo :: Integer -> Integer
>foo 0 = 16
>foo 1
>   | "Haskell" > "C++"  = 3
>   | otherwise 	     = 4
>foo n
>   | n < 0              = 0
>   | n `mod` 17 == 2    = -43
>   | otherwise          = n + 3

*i haven't verified these:
foo (-3) is 0?
foo 0 is 16
foo 1 is 3
foo 36 is -43
foo 38 is 41



Pairs:

>p :: (Int, Char)
>p = (3, 'x')

same syntax for both type and value

we can extract the elements of the pair with pattern matching:

>sumPair :: (Int,Int) -> Int
>sumPair :: (x,y) = x + y

haskell also has triples and quadruples...but apparently they shouldn't be used


Functions with multiple arguments:

>f :: Int -> Int -> Int -> Int
>f x y z = x + y + z

so the types list in the definition is an arbitrary number of input types and a single output type

be careful! function application has higher prevedence than infix operators, so put them in parentheses!



LISTS:

>nums, range, range2 :: [Integer]
>nums = [1,2,3,19]
>range = [1..100]
>range2 = [2,4..100]

List Comprehensions:

>[x*2 | x <- [1..10], x*2 >= 12]

after x is assigned to a list, it is here followed by an optional filter, only values which return true are added to the resulting list. multiple predicates can be added to the filter, each are seperated by a comma

Constructing lits:

>emptyList = []

lists can be built up using the cons operator :
cons takes an element and a lit and prepends the element to the list

>2 : 3 : 4 : [] 

is equivalent to 

>[2, 3, 4]

these are singly linked lists, not arrays

>hailstoneSeq :: Integer -> [Integer]
>hailstoneSeq 1 = [1]
>hailstoneSeq n = n : hailstoneSeq (hailstone n)

Functions on lists:

>intListLEngth :: [Integer] -> Integer
>intListLength []        = 0
>intListLength (_:xs)    = 1 + intListLength xs

note the _ wildcard "don't care" pattern, as x isn't used

nested pattern example:

>sumEveryTwo :: [Integer] -> [Integer]
>sumEveryTwo []          = []
>sumEveryTwo (x:[])      = [x]
>sumEveryTwo (x:(y:zs))  = (x + y) : sumEveryTwo zs

but in the last clause (x:y:zs) would have been equivalent... 

Combining Functions:

>hailstoneLen :: Integer -> Integer
>hailstoneLen n = intListLength (hailstoneSeq n) - 1

this is O(1) memory, it doesn't actually make the entire list, then count it, but interleaves the two operations. thank you lazy evaluation!


ERRORS:

don't be afraid of error messages!... you'll figure them out :P 



