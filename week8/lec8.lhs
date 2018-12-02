THE PROBLEM WITH PURITY

1. functions cannot have external effects
2. pures functions may not depend on inputs that change over time (filesystem, keyboard, network)


THE IO TYPE

values of "IO a" type are descriptions of effectful computations which would perform some effectful I/O operations and eventually produce a value of type "a". this type abstracts the effects from us, so that as a type we can look at it as a safe thing with no side effects. it's a description of an effectful computation.
it's like a first-class imperative program. 

imagine:

>c :: Cake

(we have a cake)

but 

>r :: Recipe Cake

means we have instructions how to make a cake, not the cake

"IO a" is a recipe for producing a value of type a, with potentially some effects along the way. 
it can be manipulated and moved like any other value in haskell, even combined with other IO values into more complex recipes

so when do these "IO a" values ever get executed? 

>main :: IO ()

^^ when this value is found, then the haskell runtime will take your "IO a" and check out the recipe.

so let's write an actual executable Haskell program:

>putStrLn :: String -> IO ()

soooo, this function, when given a String, will return an IO computation that when executed prints the String to screen, in a file "Hello.hs":

>main = putStrLn "Hello, Haskell!"

and with "runhaskell Hello.hs", the result is that the message is printed to the screen. 
"ghc --make Hello.hs" produces an executable version that can be run on demand (it's hilariously massive)

there is no "String" inside an "IO String", just a computation to eventually output a string. same as cakes and recipes

COMBINING IO

firstly, "then" ---- (>>) operator

>(>>) :: IO a -> IO b -> IO b   

this runs two IO computations in sequence, looking at the type signature, we see that the first type is not used - the output is essentially discarded, instead, it's effects are what is carried over. the effects persist and therefore the state will have changed when the second computation is run (IO b)


>main putStrLn "Hello" >> putStrLn "world!"

it's kinda like "do this, do this, do this", but if we don't want to discard the result of the firs tcomptutation, we need something different... 
we might naievly think "IO a -> IO b -> IO (a,b) could work, but this is insufficient also. we want the second computation to be able to depend on the result of the first. 
e.g., if an integer is read as input, then the second part uses the input to form an output, the second part depends on the result of the first part...

we use the bind operator (>>=) is used in this case:

>(>>=) :: IO a -> (a -> IO b) -> IO b

this... makes sense? it takes an IO a, which outputs an "a", then (a -> IO b), which uses the result of the first "IO a" in the context of "IO b" and returns IO b, which has changed depending on "a"... EASY?!

eg read a number from the user, print its successor
NB: readLn :: Read a => IO a 

>main :: IO ()
>main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))

this is kinda ugly... but we'll improve on it in the future.


RECORD SYNTAX
(not related but needed for the assignment)

suppose:
>data D = C T1 T2 T3

this could also be written as:
>data D = C { field1 :: T1, field2 ::  T2, field3 :: T3 }

this is a type along with a name that's been specified for each field in the C constructor. 
    this is functionally identical as the previous declaration and we can still construct and pattern-match on values of type D as C v1, v2, v3) but there are benefits:

1. each field name is automatically a _projection function_, which gets the value of that field out of a value of that type, here is the type:

>   field2 :: D -> T2

    we previously would have had to write this manually:

>   field2 (C _ f _) = f

    handy!!

2. there's a special syntax for constructing, modifying, pattern-matching on values of type D:
    constructing:
    
>   C { field 3 = ..., field1 = ..., field2 = ... }
    note how the fields are specified in any order! wow!

this syntax can also be used to change individusal values in an instance of the type, d :: D -

>d { field3 = ... } 

this doesn't actually change (mutate) the data, but instead creates a copy, replacing the specified value with the new data. 

we can also pattern match on records: 

>foo (C { field1 = x }) = ... x ...

this matches over field1 from a D value, obviously
