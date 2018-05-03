data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert m Leaf = Node 0 Leaf m Leaf
insert m t@(Node h l n r) 
    | ll > lr                = (Node h l n (insert m r)) 
    | lc == lf               = (Node (h+1) (insert m l) n r) 
    | otherwise              = (Node h (insert m l) n r)
    where 
        ll = leaves l
        lr = leaves r
        l2 = logBase 2.0 (fromIntegral ll)
        lc = ceiling l2
        lf = floor l2

leaves :: Tree a -> Integer
leaves Leaf                 = 1
leaves (Node _ r    _ l   ) = leaves l + leaves r
