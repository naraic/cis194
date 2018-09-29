FOLDS, AGAIN

we can use a folding function on litss, but can we generalise the idea to other types?

consider the binary tree:

>data Tree a = Empty
>            | Node (Tree a) a (Tree a)
>   derivign (Show, Eq)

>leaf :: a -> Tree a
>leaf x = Node Empty x Empty

some functions:

>treeSize :: Tree a -> Integer
>treeSize Empty        = 0
>treeSize (Node l _ r) = 1 + treeSize l + TreeSize r

>treeSum :: Tree Integer -> Integer
>treeSum Empty        = 0
>treeSum (Node l x r) = x + treeSum l + TreeSum r

>treeDepth :: Tree a -> [a]
>treeDepth Empty        = 0 
>treeDepth (Node l _ r) = 1 + max (treeDepth l + TreeDepth r)

>flatten :: Tree a -> [a]
>flatten Empty        = []
>flatten (Node l x r) = flatten l ++ [x] ++ flatten r

there's a pattern in the above functions:
    1. each takes a Tree as input
    2. pattern matches on the input Tree
    3. has a simple case for the empty answer
    4. in the Node case:
        1. calls itself recursively on both subtrees
        2. somehow combines the results from the recursive calls with the data to produce the result

so we need to abstract out this pattern!!
what changes between each example?
    1. the return type
    2. the base case
    3. the method of combination

we'll call the type of the data in the tree a and the type of the result b

>treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
>treeFold e _ Empty         = e
>treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

now! let us redefine our example functions:

>treeSize' :: Tree a -> Integer
>treeSize' = treeFold 0 (\ l _ r -> 1 + l + r)

>treeSum' :: Tree Integer -> Integer
>treeSum' = treeFold 0 (\ l x r ->  l + x + r)

>treeDepth' :: Tree a -> Integer
>treeDepth' = treeFold 0 (\ l _ r -> 1 + max l r)

>flatten' :: Tree a -> [a]
>flatten' = treeFold [] (\ l x r -> l ++ [x] ++ r)

we can write new ones easily, also!

>treeMax' :: (Ord a, Bounded a) => Tree a ->
>treeMax' = treeFold 0 (\ l x r -> l `max` x `max` r)


FOLDING EXPRESSIONS

where else have we seen folds?

from hw5:

>data ExprT = Lit Integer
>           | Add ExprT ExprT
>           | Mul ExprT ExprT

>eval :: ExprT -> Integer
>eval (Lit i)        = i
>eval (Add e1 e2)    = eval e1 + eval e2
>eval (Mul e1 e2)    = eval e1 * eval e2

we can generalise this too!

>exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
>exprTFold f _ _ (Lit i)     = f i
>exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
>exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

>eval2 :: ExprT -> Integer
>eval2 = ExprTFold id (+) (*)

now we can easily do other things, too, like count the literals in an expression:

>numLiterals :: ExprT -> Int
>numLiterals = exprTFold (const 1) (+) (+)


FOLDS IN GENERAL

the take-away message is that we can implement folds for many (not all) data types. 
the fold for T will take one (higer-order) argument for each of T's constructors, encoding how to turn the values stored by that constructor into a value of the result type - assuming that any recursive occurrences of T have already been folded into a result. many functions we might want to write on T will end up being expressible as simple folds!



MONOIDS!!!

a useful standard type class: 

>class Monoid m where
>    mempty  :: m
>    mappend :: m -> m -> m
>
>    mconcat :: [m] -> m 
>    mconcat = foldr mappend mempty

>(<>) :: Monoid m => m -> m -> m
>(<>) = mappend

types that are instances of monoid have a special element called mempty, and a binary operation "mappend" and a binary operation "mappend" abreviated "(<>)" which takes two values of the type and produces another one. the intention is that mempty is an identity for <> and <> is associateive, that is for all x, y & z:

1. mempty <> x == x
2. x <> mempty == x
3. (x <> y) <> z == x <> (y <> z)

the associativity law means that we can unambigiously write without parenthesis this:

> a <> b <> c <> d <> e


there is mconcat, also, for combining a whole lits of values, it's implemented using foldr by default, but included in the class instance since particular Monoid instances may have different ways of implemenenting it. 

lists are a monoid with concatenation:

>instance Monoid [a] where
>  mempty = []
>  mappend = (++)

addition defines a monoid on integers, but so does multiplication. we can't give two different instances of the same type class to the same type, so instead we use newtypes for each instance:
(note: newtypes can only have a single argument and are used exclusively for the compiler for type checking i.e. they have no runtime cost. also driving "Num" only makes sense in newtype definitions. it means that if "a" has an instance of Num, then "Sum a" gets an instance of Num)

>newtype Sum a = Sum a
>   deriving (Eq, Ord, Num, Show)

>getSum :: Sum a -> a 
>getSum (Sum a) = a

>instance Num a => Monoid (Sum a) where
>    mempty = Sum 0
>    mappend = (+)

>newtype Product a = Product a
>    mempty = Sum 0
>    mappend = (+)

>newtype Product a = Product a
>    deriving (Eq, Ord, Num, Show)

>getProduct :: Product a -> a
>getProduct (Product a) = a

>instance Num a => Monoid (Product a) where
>    mempty = Product 1
>    mappend = (*)

note that to find, for example, the product of a list of Integers using mconcat, wehave to first turn them into values of the type Porduct Integer:

>lst = [Integer]
>lst = [1..10]

>prod :: Integer
>prod = getProduct . mconcat . map Product $ lst

(this example is silly, since there are built-in funcions for sum and product, but it's a useful pattern) 

pairs form a monoid as long as the individual components do:

>instance (Monoid a, Monoido b) => Monoid (a,b) where
>    mempty = (mempty, mempty)
>    (a,b) `mappend` (c,d) = (a `mappend` c, b `mappend` d)

challenge: 

1. make an instance of Monoid for Bool, how many are there? 

yes, there's and, or, 

2. how would you make function types an instance of Monoid?










