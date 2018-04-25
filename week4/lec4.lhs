HIGHER-ORDER PROGRAMMING AND TYPE INFERENCE

ANONYMOUS FUNCTION

it would be convenient to not to have to give some functions names, if they will only be used once.  this frequently occurs with functions that are passed as parameters to other functions, such as with FILTER and MAP.

thus: anonymous functions...

>greaterThan100 :: [Ingeger] -> [Integer]
>greaterThan100 xs = filter (\x -> x > 100) xs

this function returns a list of Integers, all greater than 100!

the syntax begins with a backslash, which is supposed to look like a lambda, with the short leg missing.

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


