making our own types and typeclasses!!


we've see data types before, but how do we make our own? 

one way is using the "data" keyword. name and values must begin with an upper-case character:

>data Bool = False | True

this can be read as "the Bool type can have a value of True or False"


similarly, Int may be thought of in this way:

>data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

(the elipsis are for illustrative purposes and not legal code)


what about a shape? a circle might be a tuple of three floats, representing the origin (x,y) and the radius. but this could represent anything, really, so we specify a type:

>data Shape = Circle Flaot Float Float | Rectangle Float Float Float Float

these value constructors are actually functions that return a data type!

>ghci> :t Circle
>Circle :: Float -> Float -> Float -> Shape
>ghci> :t Rectangle
>Rectangle :: Float -> Float -> Float -> Float -> Shape

they are functions! just like everything else in haskell. who'da thunk it

a function that take a shape an returns its surface:

>surface :: Shape -> Float
>surface (Circle _ _ r) = pi * r ^ 2
>surface (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

looking at the the type declaration - "Shape -> Float", this couldn't have been "Circle -> Float" because Circle isn't a type (it's a constructor). same as how we couldn't specify a function with type "True -> Int".
but we _can_ pattern match against the constructors (Rectangle, Circle). 

we can use this function as expected. if we try to print out an instance of the Shape typeclass though, the interpreter doesn't know how to carry this out. this is because it doesn't know how to turn it into a string. this requires the "show" function. to automatically allow the interpreter to print the type, we can modify the code as so:

>data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

now we can enter an instance of these shapes into the interpreter and it will echo the data back at us. 


value constructors are functions, so we can use them as we normally would use functions:
>map (Circle 10 20) [4,5,6,6]

which gives us an array of new circles with the varying radii sourced from the numberic list

to improve this datatype, we make an intermediate data type that defines a point in 2D space:

>data Point = Point Float Float deriving (Show)
>data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

not that wen defining a point, we use the same name for both data type ("data Point" part of code) and for the value constructor ("Point Float Float" part). this is not special, but common, when there is only a single value constructor in the type. 
the intention for the changes to Shape is to make the meaning of the data types more obvious.
we must update "surface" to reflect the changes:

>surface :: Shape -> Float
>surface (Circle _ r) = pi * r ^ 2
>surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

note how the required changes were to the patterns! makes refactoring easier, probably!

a function to nude a shape in a given vector:

>nudge :: Shape -> Float -> Float -> Shape  
>nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
>nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  

if we didn't want to deal with points, we could make auxilliary functions that create shapes of some size on the origin and then nudge them:

>baseCircle :: Float -> Shape  
>baseCircle r = Circle (Point 0 0) r  
  
>baseRect :: Float -> Float -> Shape  
>baseRect width height = Rectangle (Point 0 0) (Point width height) 



data types can be exported in modules. the sytax is as follows:

>module Shapes 
>( Point(..)
>, Shape(..)  
>, surface  
>, nudge  
>, baseCircle  
>, baseRect  
>) where  

by writing "Shape(..)", we exported all the value constructors for Shape, so the user can make use of both Rectangle and Circle value constructors, it's equivalent to "Shape (Rectangle, Circle)".

we could also have ommited the parenthesis and its contents and forced the user to make shapes using the auxilliary functions: baseCircle & baseRect. this can be used as a form of abstraction through implementation hiding, but it does't allow the user to pattern match against the value constructors.


