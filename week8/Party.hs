module Party where
import Employee

import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = f}) (GL es fun) = GL (e:es) (fun+f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node val forest) = f val $ map (treeFold f) forest

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss guestLists = (withBoss, withoutBoss)
  where 
    withBoss = glCons boss $ mconcat $ map snd guestLists
    withoutBoss = mconcat $ map (uncurry moreFun) guestLists

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

pprint :: GuestList -> String
pprint (GL es f) = "fun " ++ show f ++ "\n" ++ employees
  where
    fun = f
    employees = unlines $ sort $ map empName es

main :: IO ()
main = do 
  company <- readFile "company.txt"
  let gl = maxFun $ read company
  putStr $ pprint gl 
