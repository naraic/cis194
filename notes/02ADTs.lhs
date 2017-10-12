Enumeration Types

>data Thing = Shoe
>           | Ship
>           | SealingWax
>           | Cabbage
>           | King
> deriving Show

this is a new type with give "data constructors" which are the only possible values of type "Thing".
"deriving Show"  tells GHC to automatically generate code for converting "Thing"s to "String"s

>shoe :: Thing
>shoe = Shoe
>
>listO'Things :: [Thing]
>listO'Things = [Shoe, SealingWax, King, Cabbage, King]
>{-
>isSmall :: Thing -> Bool
>isSmall Shoe       = True 
>isSmall Ship       = False
>isSmall SealingWax = True
>isSmall Cabbage    = True
>isSmall King       = False
>-}

which is more easily written as...

>isSmall :: Thing -> Bool
>isSmall Ship       = False
>isSmall King       = False
>isSmall _          = True

Enumeration sare only a special case of Haskell's more general "algebraic data types"
For instance, the following is not just an enumeration

>data FailableDouble = Failure
>                    | OK Double
>       deriving Show

this means that FailableDouble has two data constructors, with different number of arguments...

>ex01 = Failure
>ex02 = OK 3.4

>safeDiv :: Double -> Double -> FailableDouble
>safeDiv _ 0 = Failure
>safeDiv x y = OK (x / y)

>failureToZero :: FailableDouble -> Double
>failureToZero Failure = 0
>failureToZero (OK d)  = d

data constuctors can have more than one argument
> --store a person's name, age, favourite thing
>data Person = Person String Int Thing
>   deriving Show

>brent :: Person
>brent = Person "Brent" 31 SealingWax
>
>stan :: Person
>stan = Person "Stan" 92 Cabbage
>
>getAge :: Person -> Int
>getAge (Person _ a _) = a

the type constructor and data constructor above are both named Person but they inhabit
different namespaces and are different things. this idiom of giving type and data constuctors
the same name is common 

algebraic data types in general

ADTs in general have one or more data constructors and each can have zero or more arguments

>data AlgDataType = Constr1 Type11 Type12
>                 | Constr2 Type21
>                 | Constr3 Type31 Type32 Type33
>                 | Constr4

this means that a value of type AlgDataType can be constructed in one of four ways

NOTE: type and data constructor names must always start with a capital letter

PATTERN-MATCHING



