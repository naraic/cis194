{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
--m parameter used to track monoidal annotations to the structure
--the annotation at the root of a joinlist will always be equal to the combination of all the annotations on the Single nodes

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _)  = m


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                      = Nothing
indexJ i jl 
    | i < 0                         = Nothing
    | i > (getSize $ size $ tag jl)  = Nothing
indexJ 0 (Single m a) = Just a 
indexJ _ (Single m a) = Nothing
indexJ i (Append m jl1 jl2)
    | i > s     = indexJ (i - s) jl2 
    | otherwise = indexJ i jl1
   where s = getSize $ size $ tag jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl 
    | i < 0                             = Empty
    | i > (getSize $ size $ tag jl)     = Empty
    | i == 0                            = jl
dropJ i (Append m jl1 jl2)
    | i > s     = dropJ (i - s) jl2
    | otherwise = (dropJ i jl1) +++ jl2
    where s = getSize $ size $ tag jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl | i <= 0 = Empty
           | (getSize $ size $ tag jl) > i = jl
takeJ i (Append m jl1 jl2)
    | i > s     = jl1 +++ takeJ (i - s) jl2
    | otherwise = takeJ i jl1
    where s = getSize $ size $ tag jl1

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

jlToString :: JoinList m String -> String
jlToString Empty = ""
jlToString (Single _ s) = s
jlToString (Append _ jl1 jl2) = jlToString jl1 ++ jlToString jl2

instance Buffer (JoinList (Score, Size) String) where
    toString   = jlToString
    fromString s = Single (scoreString s, (Size 1)) s
    line = indexJ
    replaceLine n s jl = takeJ (n-1) jl +++ fromString s +++ dropJ n jl 
    numLines = getSize . snd . tag 
    value = getScore . fst . tag 
