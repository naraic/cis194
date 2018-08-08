LAZY EVALUATION


suggested reading: foldr foldl foldl' - from haskell wiki - see fold.lhs


STRICT EVALUATION:

before looking at lazy evaluation, it is useful to look at some examples of the opposite, first:
strict evaluation. 

under a strict evaluation strategy, function arguments are completely evaluated before passing them to the function.

this is useful if we want to predict when and in what order things will happen. in strict languages, you normally know if arguments are evaluated from left to right or visa-versa, for example.

it sometimes leads to wasted computation though, for example, if an argument is evaluated, but not actually used in the function body this would be wasted computation.

if side effects are allowed, strict evaluation is clearer and more predictable. 

SIDE EFFECTS AND PURITY

so the real issue might be the presence or absence of side effects?
side effects are anything that causes evaluation of an expression to interact with something outside itself. such interactions may be time sensitive. eg:

modifying a global variable 
printing to the screen (screen state changes continuously)
reading a file or from the network

so if lazy evaluation makes it difficult to reason about when things will be evaluated, it makes dealing with effects difficult in a lazy language.

a language with no side effects would not be very useful. it would only allow interpretation and evaluation of expressions, without user input or printing to the screen or reading from a file. 
the solution to this is the IO monad, which will be discussed in a later lesson.


LAZY EVALUATION

under a lazy evaluation strategy, evaluation of function arguments is delayed as long as possible. they are not evaluated until it is necessary to do so.
when an expression is given as a function arguemnt is is packaged up as an unevaluated expression, called a thunk.
the thunk is discarded by the garbage collector if it is unused after a function has been run. 


PATTERN MATCHING DRIVES EVALUATION

when is expression oevaluation necessary? it's not quite always just when it's _used_ in the function, but instead... consider the following:

>f1 :: Maybe a -> [Maybe a]
>f1 m = [m,m]

>f2 :: Maybe a -> [a]
>f2 Nothing = []
>f2 (Just x) = [x]

f1 and f2 both USE their arguements, but while f1 uses "m" it needs to know nothing about it. it may remain completely unevaluated. f2, conversely, needs to determine whether m is a Just or a Nothing. therefore the arguemnt must be evaluated for pattern-matching to take place. 

also, the thunks are actually only evaluated enough so that a pattern match may proceed, and no further. 

remember: "pattern matching drives evaluation"
    - expressions are only evaluated when pattern-matched
    - only as far as necessary for the match to proceed and no further

let's look at the evaluation of "take 3 (repeat 7):

>repeat :: a -> [a]
>repeat x = x : repeat x

>take :: Int -> [a] -> [a]
>take n _      | n <= 0 = []
>take _ []              = []
>take n (x:xs)          = x : take (n-1) xs
 

  take 3 (repeat 7)
      { 3 <= 0 is False, so we proceed to the second clause, which
    needs to match on the second argument. So we must expand
    repeat 7 one step. }
= take 3 (7 : repeat 7)
      { the second clause does not match but the third clause
        does. Note that (3-1) does not get evaluated yet! }
= 7 : take (3-1) (repeat 7)
      { In order to decide on the first clause, we must test (3-1)
        <= 0 which requires evaluating (3-1). }
= 7 : take 2 (repeat 7)
      { 2 <= 0 is False, so we must expand repeat 7 again. }
= 7 : take 2 (7 : repeat 7)
      { The rest is similar. }
= 7 : 7 : take (2-1) (repeat 7)
= 7 : 7 : take 1 (repeat 7)
= 7 : 7 : take 1 (7 : repeat 7)
= 7 : 7 : 7 : take (1-1) (repeat 7)
= 7 : 7 : 7 : take 0 (repeat 7)
= 7 : 7 : 7 : []

**although the above evaluation is possible, haskell compilers are actually a little more sophisticated, and GHC uses a technique called graph reduction, where the expression being evaluated is represented as a graph, so different parts can share pointers to the same subexpression. this ensures work is not duplicated unnecessarily. 


CONSEQUENCES

laziness has consequences:

-PURITY
	lazy evaluation forces purity (pretty much)

-UNDERSTANDING SPACE USAGE
	laziness has a downside that it can be difficult to reason about the space usage of programs:
>--std lib foldl
>foldl :: (b -> a -> b) -> b -> [a] -> b
>foldl _ z []		= z
>foldl f z (x:xs) 	= foldl f (f z x) xs
	
	let's evaluate foldl (+) 0 [1,2,3]:

	foldl (+) 0 [1,2,3]
  = foldl (+) (0+1) [2,3]
  = foldl (+) ((0+1)+2) [3]
  = foldl (+) (((0+1)+2)+3) []
  = (((0+1)+2)+3)
  = (3+3)
  = 6

since the value of the accumulator is not demanded until recursing through the entir elist, the accumulator simply builds up a big unevaluated expression, which finally gets reduced to a value. 
there are at least two problems with this 
	- the time requirement - the expansion _then_ the reduction of the list onto the stack is essentially doing the same thing twice. 
	- the second is stack overflows, due to pushing each item from the lits onto the stack in a partial sum

the solution to this is foldl'

SHORT-CIRCUITING OPERATORS:

logical operators in other languages are often short-circuiting, so if the first value in an AND expression is found to be false, the other variables are not evalutated since it won't change the outcome of the logical statement. 
this is against the normal language standards though, of say C and java where the opposite is normally the case when it comes to function arguemnts. 

in haskell, we can (re)define this behavior without a special case:

>(&&) :: Bool -> Bool -> Bool
>True  && x  = x
>False && _ = False 

(&&) could also have been defined like this:

>(&&!) :: Bool -> Bool -> Bool
>True &&! True = True
>True &&! False = False
>False &&! True = False
>False &&! False = False

this version takes the same vales as (&&), but has different behavior:

>False && (34^997 > 928374)
>False &&! (34^997 > 928374)

both will evaluate to false but the latter will take much longer

>False && (head [] == 'x')
>False &&! (head [] == 'x')

the first will be False and the second will crash

basically, these show there are considerations to be made when designing a function with lazy evaluation


USER-DEFINED CONTROL STRUCTURES

taking the idea of short-circuiting operators one step further, we can define our own control structures in haskell

most languages have a special build-in "if" construct. this makes sense because only one of the two potential branches should be evaluated.

however, this is possible to define with haskell:

>if' :: Bool -> a -> a -> a
>if' True x _ = x
>if' False _ y = y

if doesn't get used much in haskell, anyway, pattern-matching guards are usually used instead. 
other control structures can also be defined - this will be explored alongside monads.


INFINITE DATA STRUCTURES

for example, "repeat x". when defined they first exist as a thunk, and as needed the calling function evaluates each further element. it's as if it's a seed out of which further data can grow.


PIPELINED/WHOLEMEAL PROGRAMMING

pipelined incremental transformations of a large data structure can be memory-efficient. we can now see why - due to laziness each stage of the pipeline can operate in lockstep, only generating each bit of the resulat as it is demaneded by the next stage of the pipeline.


DYNAMIC PROGRAMMING

we can get haskell to determine routes in dynamic programming itself * * * READ MORE ON THIS
