module TopSortTest where

import Data.List

import Digraph

exampleGood = foldl' (\g (a,b) -> add_edge g a b)
                     (makeGraphWithNodes ["1", "21", "22", "333"])
                     [("1", "21"), ("1", "22"), ("21", "333"), ("22", "333")]

exampleBad =  foldl' (\g (a,b) -> add_edge g a b)
                     (makeGraphWithNodes [1..3])
                     [(1,2),(2,3),(3,1)]

