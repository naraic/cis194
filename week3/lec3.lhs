"RECURSION PATTERNS, POLYMORPHISM AND THE PRELUDE"

experienced haskell programmers hardly ever write recursive functions! (?) 

the key to achieving this is by noticing that although recursive functions can theoretically do anything, but there are common patterns that recurr. by abstracting these patterns into functions we can abstract the recursiion based patterns - wholemeal programming!


Recursion patterns:

recall:
>data IntList = Empty | Cons Int IntList
>   deriving Show


so what are some sommon procedures we might perform on an IntList?

    -an operation on every item of the list
    -keep only some elements in the lits based on some test
    -"summarize" the elements of the list using some operation
    -other stuff



MAP

-perform some operation on every element of the lits

example:

make sure the lit is non-negative

>absAll :: IntList -> IntList
>absAll Empty = Empty
>absAll (Cons x xs) = Cons (abs x) (absAll xs)

or... square each element

>squareAll :: IntList -> IntList
>squareAll Empty     = Empty
>squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

see the repetition? this is annoying. let's get rid of it. wholemeal.

what changes between the functions is the the operation that's applied to x.
we can specify this as a function with type Int -> Int
this is why functions as parameters is useful.

we will call the function which applies the parameterised function mapIntList

>mapIntList -> (Int -> Int) -> IntList -> Intlist
>mapIntList _  Empty = Empty
>mapIntList f (Cons x xs) = Cons (f x) mapIntList f xs

but if it is correct.... 

>exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

>addone x = x + 1
>square x = x * x

>mapIntList addone exampleList
>mapIntList abs exampleList
>mapIntList square exampleList



FILTER

-keep only some elements in list based on a test. e.g. positive numbers

>keepOnlyEven :: IntList -> IntList
>keepOnlyEven :: Empty = Empty
>keepOnlyEven (Cons x xs)
>    | even x = Cons x (keepOnlyEven xs)
>    | otherwise = keepOnlyEven xs

so we might be able to generalise the above function with something like this:

>mapIntList -> (Int -> Int) -> IntList -> IntList 
>mapIntList _ Empty = Empty
>mapIntList (Cons x xs)
>   | f x = Cons x (mapIntList xs)
>   | otherwise = mapIntList xs


FOLD

here we summarise the elements of a lits, it's also known as reduce.
it will be discussed in more detail in the next lecture. 

might look like this? 

>foldIntList -> (Int -> Int -> Int) -> IntList -> Int
>foldIntList f Empty = f 0 0
>foldIntList f (Cons x xs) = f x (foldIntList xs)

(i think this might actually work but there's probably a more elegant solution)



POLYMORPHISM

so far these have been nice general functions for mapping and filtering over lists of Ints, but we can generalise them more. 
if we want to filter lists of different types we'd need to rewrite the above functions, so maybe we can get more wholemeal? :P
if we were to rewrite the above code fore each new type we wish to use it on, the only thing that would actually change is the type signatures, which is kinda cool, but it also means that the bodies of the functions would be identical which means... lots of repetition, which is, like, totally uncool.

to get deal with this, haskell provides polymorphism, for both data types and functions.
"polymorphic" comes from the Greek (πολύμορφος) and means "having many forms": something which is polymorphic works for multiple types.


Polymorphic data types

declare a polymorphic datatype:

>data List t = E | C t (List t)
(we're using "E" and "C" instead of Empty and Cons as they've already been used in IntList)
note that we're now using "t" as a TYPE VARIABLE. type variables must begin with a lower case letter. a type variable may represent any type.
"data List t = ..." means that the "List" type is parameterised by a type, similarly to how a function can be marameterised by some input.

given a type, "t", a (List t) consists of either the constructor E, or the constructor C along with a value of type "t" and another (List t). examples:

>lst1 :: List Int
>lst1 = C 3 (C 3 (C 5 (C 2 E)))

>lst2 :: List Char
>lst2 = C 'x' (C 'y' (C 'z'  E))

>lst3 :: List Bool
>lst3 = C True (C False E)


POLYMORPHIC FUNCTIONS

;et's generalise filterIntList to work over our new polymorphic "Lists", we can just take the previous code and replace Empty by E and Cons by C:

>filterList _ E = E
>filterList p (C x xs)
>   | p x       = C x (filterList p xs)
>   | oterwise  = filterList p xs

now what's the type of filterList? checking it in ghci, it infers:

*Main> :t filterList
filterList :: (t -> Bool) -> List t -> List t

we can read this as "for any type t, filterList takes a function from t to Bool, and a list of t's and returns a list of t's"

when converting the mapList function to be polymetric we might produce something along the lines of:
>mapList :: (t -> t) -> List t -> List t

while this is suitable, it may be overly restrictive, as it returns the output type to be the same as the input type. we can allow for a different input type by changing the name of the input and output type variables, like so:

>mapList :: (a -> b) -> List a -> List b
>mapList _ E        = E
>mapList f (C x xs) = C (f x) (mapList f xs)

an noteworthy aspect of polymorphic functions is that the caller gets to pick the types. when a polymorphic function is written it must work for every possible input type. this - and based on the fact that haskell has no way of driectly making decisions based on the type of something - has some interesting inmplications which will later be explored.

THE PRELUDE

the "Prelude" is a module with a bunch of standard definitions that gets implicitly imported into every haskell program. check out its documentation @ https://hackage.haskell.org/package/base/docs/Prelude.html

polymorphic lists are defined in the Prelude, along with many polymorphic functions for working with them, e.g. "filter" and "map".

another useful polymorphic type is Maybe, defined:

>data Maybe a = Nothing | Just a

a value of type "Maybe a" either contains a value of type a (wrapped in the Just constructor) or it is "Nothing" (which represents a failure or effor).  Data.Maybe module has functions for working with this type.


TOTAL AND PARTIAL FUNCTIONS

consider this polymorphic type:

[a] -> a

what functions could have such a type? perhaps, the "head" function, which returns the first element of a list?

...but what if the input is an empty list?
it returns an error. 

head is a "PARTIAL FUNCTION", there are inputs for which it will crash. functions with inputs that will make them recurse infinitely are also called partial. functions which are well-defined on all possible inputs are known as "TOTAL FUNCTIONS".

it's good practice to avoid partial functions as much as possible. 
other partial functions include tail, init, last and (!!) and they should be avoided, even though they're in the Prelude.

what should we do intead?

REPLACING PARTIAL FUNCTIONS

they can often be replaced by patter-matching. consider:

>doStuff1 :: [Int] -> Int
>doStuff1 []  =  0
>doStuff1 [_]  =  0
>doStuff1 xs = head xs + (head (tail xs)) 

>doStuff2 :: [Int] -> Int
>doStuff2 []  =  0
>doStuff2 [_]  =  0
>doStuff2 (x1:x2:_) = x1 + x2

these both do the same thing and are both total, but the second is probably easier to read.

WRITING PARTIAL FUNCTIONS

if you find yourself writing a partial function, how should it be aproached? two ways:

1. change the output of the function to indicate possible failure:
    conside Maybe:

>data Maybe a = Nothing | Just a

now suppose we're rewriting head:

>safeHead :: [a] -> Maybe a
>safeHead []     = Nothing
>safeHead (x: _) = Just x

why is this a good idea?
    1. safeHead will never crash
    2. the type of safeHead makes it obvious that it may fail for some functions
    3. the type systems ensures that users of safeHead must appropriately check the return value of safeHead to see whether they got a value or Nothin

in some sense, it's safeHead is still partial, but this is reflected in the type system and is safe.

what if we know that the unsafe function will only be used with safe inputs? then it's possibly annoying to get a "Maybe" result, since we know it's never going to matter. 
the answer: if a condition really is guaranteed, it should be reflected in the types, then the compiler can enforce your guarantees for you. eg:

>data NonEmpty List a = NEL a [a]

>nelToList :: NonEmptyList a -> [a]
>nelToList (NEL x xs) = x:xs

>listToNel :: [a] -> Maybe (NonEmptyList a)
>listToNel []        = Nothing
>listToNel (x:xs)    = Just $ NEL x xs

>headNEL :: NonEmptyList a -> a
>headNEL (NEL a _) = a

>tailNEL :: NonEmmptyList a -> [a]
>tailNel (NEL _ as) = as


