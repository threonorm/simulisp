module TopSortTest where

import Data.List
import qualified Data.Map as Map

import Test.QuickCheck

import Digraph


-- Simple examples to test the topological sort

exampleGood = foldl' (\g (a,b) -> addEdge g a b)
                     (makeGraphWithNodes ["1", "21", "22", "333"])
                     [("1", "21"), ("1", "22"), ("21", "333"), ("22", "333")]

exampleBad =  foldl' (\g (a,b) -> addEdge g a b)
                     (makeGraphWithNodes ([1..3] :: [Int]))
                     [(1,2),(2,3),(3,1)]

-- Automated test generation: QuickCheck

testTopSort = quickCheck (prop :: (Graph Int) -> Bool)

-- Two cases:
-- * cyclic graph -> rejection
-- * acyclic graph -> returns correct linearization of partial order
prop g | hasCycle g = topological g == Nothing
       | not (hasCycle g), Just topSort <- topological g =
           let locallyCorrect node successors =
                     all (\s -> position node < position s) successors
               position (Node n) = positionMap Map.! n
               positionMap = Map.fromList $ zip topSort [1..]
           in allWithKey locallyCorrect $ g_edges_to g
       | otherwise = False
              

allWithKey :: (k -> a -> Bool) -> Map.Map k a -> Bool
allWithKey p = Map.foldr (&&) True . Map.mapWithKey p
