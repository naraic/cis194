NOT ABSTRACT!!! DATA TYPES :O

_ALGEBRAIC_ Data Types


Enumeration Types

>data Thing = Shoe
>           | Ship
>           | SealingWax
>           | Cabbage
>           | King
>   deriving Show

here we have declared a type, "Thing" with five data constructors, these are "Thing"'s only values.
"deriving Show" tells GHC to automatically generate code for converting "Things" to "Strings".


>shoe :: Thing
>shoe = Shoe

>listO'Things :: [Thing]
>listO'Things = [Shoe, SealingWax, King, Cabbage, King]

pattern matching can be used to distinguish enums:

>isSmall :: Thing -> Bool
>isSmall Shoe       = True
>isSmall Ship       = False
>isSmall SealingWax = True
>isSmall Cabbage    = True
>isSmall King       = False
              
or more concisely: 

>isSmall2 :: Thing -> Bool
>isSmall2 Ship = False
>isSmall2 King = False
>isSmall2 _    = True


-Beyond Enumerations

they're really just a special case of Haskells ADTs, not like enums in more common languages

>data FailableDouble = Failure
>                    | OK Double
>    deriving Show

>ex01 = Failure 
>ex02 = OK 3.4

this is kinda a polymorphic declaration, i guess? so if there are no parameters, the type becomes "FailableDouble" with value "Failure", if there is one double parameter then the value is OK with the supplied double.

the type of "OK" is probably... FailableDouble -> Double?

>safeDiv :: Double -> Double -> FailableDouble
>saveDiv _ 0 = Failure
>saveDiv x y = OK (x / y)

>failureToZero :: FailableDouble -> Double
>failureToZero Failure = 0
>failureToZero (OK d) = d


Data constructors may have multiple arguments:
(name, age, favourite thing)

>data Person = Person String Int Thing
>    deriving Show

>brent :: Person
>brent = Person "Brent" 31 SealingWax

>stan :: Person
>stan = Person "Stan" 94 Cabbage

>getAge :: Person -> Int
>getAge (Person _ a _) = a

see in getAge how the type constructor and data constructor are both "Person", but inhabit different namespaces and are different things. This idiom of giving type and data contrcutor of a one-constructor type the same name is common. 


Algebraic data types in GENERAL

-they hae one or more data constructors 
-each constructor has zero or more arguments

>data AlgDataType = Constr1 Type11 Type12
>                 | Constr2 Type21
>                 | Constr3 Type31 Type32 Type33
>                 | Constr4

the above exampe shows that AlgDataType can be constructed in 4 ways, each with a different number of values

NOTE: type and data constructor names must start with a capital letter.
      variables (and function names) must always start with a lower case letter.


-PATTERN MATCHING

this is basically about taking apart a value by figuring out what constructor built it. or determining it's shape, maybe? it's the only way to make decisions in haskell...

eg, using AlgDataType:

foo (Constr1 a b)   = ...
foo (Constr2 a)     = ...
foo (Constr3 a b c) = ...
foo Constr4         = ...

see above how the values that come with each constructor can be named as desired, and that when a constructor has any values the pattern must be enclosed within parenthesis.

NOTE:

    1 _ - underscore is a "wildcard pattern" and will match anything.

    2 x@pattern can be used to name "pattern" x e.g.

>baz :: Person -> String
>baz p@(Person n _ _) + "The name of the field of (" ++ show p ++ ") is " ++ n

"baz brent" returns: "The name field of (Person \"Brent\" 31 SealingWax) is Brent"

    3 patterns can be nested:

>checkFav :: Person -> String
>checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
>checkFav (Person n _ _)          = n ++ ", your favourite thing is lame!"


PATTERN GRAMMAR:

pat ::= _
     |  var
     |  var @ ( pat )
     |  ( Constructor pat1 pat2 ... patn )

     so... wildcard is a pattern.
     any solitary variable by itself is a pattern
     @ patterns are a thing
     and finally, a constructor name followed by a sequence of patterns is a pattern

     
     NOTE: 
        literal values are equivalent to constructors with no arguements, as if:

        data Int = 0 | 1 | -1 | 2 | -2 | ...
        data Char = 'a' | 'b' | 'c' | ... which allows pattern-matching against literals.
         
        * THIS IS NOT ACTUALLY HOW Int & Char ARE DEFINED!! *



CASE EXPRESSIONS:

the underlying construct of pattern-matching is the "case" expression:

    case exp of
        pat1 -> exp1
        pat2 -> exp2
        ...

    when evaluated 'exp' is matched against each of teh patterns pat1, pat2, ... in turn and the first matching pattern is chosen and the corresponding expression is evaluated, e.g.:

>ex03 = case "Hello" of
>           []      -> 3
>           ('H':s) -> length s
>           _       -> 7

...evaluates to 4

so function definition is mostly syntactic sugar for case expression definitions e.g.:

>failureToZero' :: FailableDouble -> Double
>failureToZero' x = case x of
>                        Failure -> 0
>                        OK d    -> d


RECURSIVE DATA TYPES

we have seen this with lists already - a list is either empty or a single element followed by a remaining list

we could redefine the list like so:

>data IntList = Empty | Cons Int IntList

the built-in litss are quite similar, but they've got special syntax ([] and :)

we often use recusive functions for dealing with recusive data types

>intListProd :: IntList -> Int
>intListProd Empty       = 1
>intListProd (Cons x l)  = x * intListProd l

or we could define a binary tree with an Int at each node and a Char at each leaf:

>data Tree = Leaf Char
>          | Node Tree Int Tree
>   deriving Show

>tree :: Tree
>tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))



    

