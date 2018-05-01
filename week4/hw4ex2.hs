data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert m Leaf = Node 0 Leaf m Leaf
insert m (Node h Leaf n Leaf)   = Node (h+1) (insert m Leaf) n Leaf 
insert m (Node h l n Leaf)      = Node h l n (insert m Leaf)
insert m (Node h l@(Node lh _ _ _) n r@(Node rh _ _ _)) -- with lh satanding for left height etc
    | lh == rh  = (Node (h+1) (insert m l) n r)
    | otherwise = (Node h l n (insert m r))





--foldr :: (a -> b -> b) -> b -> t a -> b 
--foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
