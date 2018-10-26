MOTIVATION

so we see a lot of functions, like map, that pass a function over a structure, and they have various forms, which vary depending on the design of the structure.

perhaps there's a generalised abstraction to be found here! 

so usually our map functions look like this:

>thingMap :: (a -> b) -> f a -> f b

but usually the "f" in our map is a container. a description of a structure of some sort. can we assign a type variable, like "f", to them?

A BRIEF DIGRESSION ON KINDS

ever expression has a type, but types themselves have "types" called KINDS.
you can find out about the kind of a type using ":kind":

> :k Int
>Int :: *

so we see here that Int has kind *
this is true for all types that serve as values, e.g. Bool, Char, Maybe Int

hmmm... Maybe Int? but Maybe alone has no values. there are values of Maybe when it has been combined with another type, but not when it is alone...

> :k Maybe
>Maybe :: * -> *

so the kind of Maybe, is a FUNCTION ON TYPES, or usually a TYPE CONSTRUCTOR. 
so maybe takes an input type of kind * and produces another type of kind *. eg: input Int :: * and get Maybe Int :: *

what other tpye constructors are of kind * -> *? 
[] and Tree


what about type constructors with different kinds?
e.g. JoinList from hw7

>data JoinList m a = Empty
>                  | Single m a
>                  | Appennd m (JoinList m a) (JoinList m a)

> :k JoinList
> JoinList :: * -> * -> *

so JoinList expects two type parameters and returns a new type. 
it's also curried though, so we could think of it as taking one type and giving back something of kind * -> *, either!

> :k (->)
>(->) :: * -> * -> *

this tells us the function type constructor takes two arguments. 

what about:

>data Funny f a = Funny a (f a)

> :k Funny
> Funny :: (* -> *) -> * -> *

it's a higher order type constructor, so types can be partially applied too, just like functions.


FUNCTOR

earlier we decided we needed something like this:

>thingMap :: (a -> b) -> f a -> f b

where f is a type variable standing in for some type of kind * -> *
so can we write it like this?

>thingMap :: (a -> b) -> f a -> f b
>thingMap h fa = ???

not really :( we can't do much if we don't know what f is. it needs to work differently for each "f"
so we make a type class, which is normally called Functor:

>class Functor f where
>  fmap :: (a -> b) -> f a -> f b

functor is defined in standard prelude and the name come from category theory and carries no relation to C++ functors. 
so now we must implement this class in a specific way for each particular f. 
NOTE: Functor class abstracts over types of kind * -> * so we can't write:

>instance Fuctor Int where
>  fmap = ...

if we do, we get a kind mismatch error!

but we could write an instance of Functor for... Maybe, for example:

>instance Functor Maybe where
>  fmap _ Nothing  = Nothing
>  fmap h (Just a) = Just (h a)

or 

>instance Functor [] where
>  fmap _ []      = []
>  fmap f (x:xs)  = f x : fmap f xs
> -- or just
> -- fmap = map


but what about IO, does it make sense to define a functor over IO?
yes, 
>fmap :: (a -> b) -> IO a -> IO b 
this results in the IO action which first runs the "IO a" action, then applies the function to transform the result before returning it...

>instance Functor IO where
>  fmap f ioa = ioa >>= (\a -> return (f a))

OR

>instance Functor IO where
>  fmap f ioa = ioa >>= (return . f)

now to confuse us:

>instance Functor ((->) e) where
following the types our definition would be
>fmap :: (a -> b) -> (->) e a -> (->) e b
or with infix:
>fmap :: (a -> b) -> (e -> a) -> (e -> b)
but this type signature is familiar:
>instance Functor ((->) e) where
>  fmap = (.)

but what does this meeaaaannnnn???? well, we could think of (e -> a) as an e-indexed container, with one value of a for each value of e.   
to map a function over every value in such a container corresponds exactly to function composition:
to pick an element out of the transformed container, we first apple (e -> a) function to pick an element of the transformed container then apply the (a -> b) function to transform the element we picked....

WHAT
