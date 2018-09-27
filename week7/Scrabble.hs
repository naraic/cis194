{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c = checkout $ lookup (toLower c) al
    where
        al = [('a',1),('e',1),('i',1),('l',1),('n',1),('o',1),('r',1),('s',1),('t',1),('u',1),
              ('d',2),('g',2),
              ('b',3),('c',3),('p',3),('m',3),
              ('f',4),('h',4),('v',4),('w',4),('y',4),
              ('k',5),
              ('j',8),('x',8),
              ('q',10),('z',10)]

checkout :: Maybe Int -> Score
checkout Nothing = Score 0
checkout (Just n) = Score n

scoreString :: String -> Score
scoreString s = mconcat $ fmap score s

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)
