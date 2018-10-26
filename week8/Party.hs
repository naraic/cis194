module Party where

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = f}) (GL es fun) = GL (e:es) (fun+f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
    | f1 > f2 = gl1
    | otherwise = gl2

treeFold :: (Tree a -> b -> b) -> b -> Tree a -> b
treeFold f z (Node val forest)
    | forest == [] = z
    | otherwise = f val . foldr (treeFold f z) z forest



