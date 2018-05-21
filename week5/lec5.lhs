MORE POLYMORPHISM AND TYPE CLASSES

haskell's polymorphism is known as parametric polymorphism.
this means polymorphic functions must work uniformly for any input type.


PARAMETRICITY

consider the type:

>a -> a -> a

a is a type variable, so what functions could have this type?

maybe:

>f :: a -> a -> a
>f x y = x && y

NO!! this doesn't work, as "&&" function is not defined for all types, which is what "a" represents in our type signature. 

there's an error message. 'a' is a rigid type variable, not just 'Bool', which is what "&&" takes as a parameter!


the reason for this is the _caller_ of a polymetric function gets to choose the type.
in this example we are the implementors, and we have tried to use a specific type in our code, "Bool", which is specified by "&&", the caller of the function may have wanted to pass a "String" or "Int" or any type at all, which is _legitimate_ usage, looking at our type signature, but not in our code.

the type "a -> a -> a" is a promise, that the function will work no matter what type the caller chooses.

we might like to imagine an implementation that uses a function which can determine the type of the passed parameter at runtime, such as "typeOf" or "instanceOf", but such functions DO NOT EXIST in haskell!! 
in fact, these functions would be impossible to implement in haskell, because haskell erases the types after compilation and at runtime the types no longer exist! there are reasons for this we do not yet understand.


this is known as PARAMETRIC POLYMORPHISM. we say f :: a -> a -> a is parametric in type a.
parametric, here, is used to mean "works uniformly for any type chosen by the caller".
this is provided by "generics" in java, which were actually inspired by haskell.

so, what functions would work, with our "a -> a -> a" type?
there are actually only two! (we really can determine a lot from the type signature!"

>f1 :: a -> a -> a
>f1 x y = x 

>f2 :: a -> a -> a
>f2 x y = y 

ponder the following types and their possible functions:

a -> a          -- just returns a - called "id"
a -> b          -- returns a different type, probably not related to an operation on the 'a'. (unsafeCoerce). so it allows something that probably shouldn't be allowed...  
a -> b -> a     -- ignores b, returns a - actually const! from hoogle. interesting, i was right though.
[a] -> [a]      -- manipulates a list somehow. any general non-reductive list operation, probably
(b -> c) -> (a -> b) -> (a -> c) -- f . o . g ? 
(a -> a) -> a -> a -- function operating on a, applied to second parameter a. very general.


TWO VIEWS ON PARAMETRICITY

coming from java, as a writer of polymorphic functions, this system might seem restrictive.

as a user of functions though, this makes things much more convenient! instead of worrying that the function that you're using is correctly handling the type of the data that you're passing as a parameter, it's instead been guaranteed by the compiler to work for all types.
these are what we call "STRONG GUARANTEES"

but, it's so useful to be able to decide what to do based on types! like with addition, it works on multiple, but not all types! like "Int, Integer, Double" etc, and it's clearly doing different stuff based on the type!

to get to the bottom of this, let's look at (+) :

Prelude> :t (+)
(+) :: Num a => a -> a -> a

what is this "Num a =>" thing?

a similar signature also appears:

>(==) :: Eq a   => a -> a -> Bool
>(<)  :: Ord a  => a -> a -> Bool
>show :: Show a => a -> String

these are: 

TYPE CLASSES!!

Num, Eq, Ord, Show are "type classes" and (==) (<) (+) are "type class polymorphic"
type classes correspond to "sets of types" which have certain operations defined for them and type class polymorphic functions work only for types which are instances of the type class(es) in question.

>class Eq a where
>   (==) :: a -> a -> Bool
>   (/=) :: a -> a -> Bool

we can read this as:
    "Eq" is a type class with a single parameter, 'a'
    any type wanting to be an instance of "Eq" must define two functions, (==) and (/=) - with the indicated type signatures.

    eg: to make Int an instance of Eq we would have to define (==) :: Int -> Int -> Bool and (/=) :: Int -> Int -> Bool (already defined in prelude)

so, the type of (==):

>(==) :: Eq a -> a -> a -> Bool

the Eq that comes before the "=>" is a "type class constraint" - which defines: for any type a, as long as a is an instance of Eq, (==) can take two values of type a and return a Bool. 
it is a "type error" to call (==) on a non Eq type.

TYPE INFERENCE is used to figure out what implementation of (==) should be used, based on the type. 
similar to method overloading in other languages.

example:

>data Foo = F Int | G Char

>instance Eq Foo where
>   (F i1) == (F i2) = i1 == i2
>   (G c1) == (G c2) = c1 == c2
>   _ == _ = False

>foo1 /= foo2 = not (foo1 == foo2)

defining both (==) and (/=) seems almost repetitive - so haskell type classes offer "default implementations" of methods in terms of other methods. these should be used whenever an instance does not override the default definition on its own.

imagine Eq being defined as:

>class Eq a where
>   (==) :: a -> a -> Bool
>   (/=) :: a -> a -> Bool
>   x /= y = not (x == y)

now, when an instance of Eq is declared, only the (==) function needs to be implemented, since there's a default implementation of (/=) given in the default type class implementation

BUT REALLY... it looks more like this:

>class Eq a where
>    (==), (/=) :: a -> a -> Bool
>    x == y = not (x /= y)
>    x /= y = not (x == y)

with both (==) and (/=) defined in the default implementation, when an instance of Eq is written, it can be defined in terms of either (==) or (/=), and the other will be generated automagically. but if neither is declared we instead get infinite recursion. safe.

GHC is actually able to automatically generate instances of Eq, along with Ord, Show and a few others...

>data Foo' = F' Int | G' Char
> deriving (Eq, Ord, Show)


COMPARING TYPE CLASSES WITH JAVA INTERFACES

1. java classes must have the interfaces it implements declared at their definition. haskell type class instances, can be declared separately from the corresponding type declaration or put into a separate module.

2. types that can be specified for type class methods are more general and flexibale than the signaures that can be given for java interface methods, especially when multi-parameter type classes enter the picture - eg, 

>class Blerg a b where
>  Blerg :: a -> b -> Bool

using blerg amounts to doing "multiple dispatch": which implementation of blerg the compiler should cooose depends on the types of both a and b, there's no easy way to do this in java. 
haskell type classes can also easily handle binary (or ternary etc) methods as in:

>class Num a where
> (+) :: a -> a -> a
> ...


there's no nice way to do this in java, either, one argument would need to be privileged, which means getting (+) invoked on it and the asymmetry is awkward. becuase of subtyping in java, two aguments of a certain interface type also does not guarantee that they're actually the same type... so that just messes everything up.


STANDARD TYPE CLASSES

Ord - they can be compared with the usual comparison operators, and "compare" - can be "totally ordered"

Num - "numberic" types, support addition, subtraction, multiplication. notably, integer literals are type class polymorphic:

Prelude> :t 5
5 :: Num a => a

so literals can be used as any type that is an instance of Num

Show - convertable to Strings for display

Read - dual of Show

Integral - whole number types - Int and Integer


example:

>class Listable a where
>    toList :: a -> [Int]

Listable is the class of things which can be converted into a list of Ints...

>toList :: Listable a => a -> [Int]

>instance Listable Int where
    -- toList :: Int -> [Int]
>    toList x = [x]


>instance Listable Bool where
>    toList True = [1]
>    toList False = [0]
>
>instance Listable [Int] hwere
>    toList = id

here's a binary tree we can convert to a list by flattening

>data Tree a - Empty | Node a (Tree a) (Tree a)
>
>instance Listable (Tree Int) where
>    toList Empty        = []
>    toList (Node x l r) = toList l ++ [x] ++ toList r


if we implement other functions in terms of toList, they also get a Listable constraint:

>sumL x = sum(toLisit x)

ghci tells us the type of sumL:

>sumL :: Listable a => a -> Int

because sumL will only work on types that are instances of Listable, since it uses toList

>foo x y - sum (toList x) == sum (toList y) || x < y

ghci tells the type:

>foo :: (Listable, Ord a) => a -> a -> Bool

so x, y need to be instances of both Ord and Listable

finally:

>insance (Listable a, Listable b) => Listable (a,b) where
>    toList (x, y) = toList x ++ toList y

we put type class constraints on an instance as well as on a function type... 
this says that a pair type (a,b) is an instance of Listable as long as a and b both are. 
then we get to use toList on values of types a and b in our definiton of toList for a pair. not that this definition is not recursive. we are defining a version of toList that calls other versions of toList, not itself!!
