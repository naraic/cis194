module Main where

import JoinList
import Editor
import Scrabble
import Sized

main = runEditor editor (Single ((scoreString s), (Size 1)) s)
        where s = "Hello world!"
