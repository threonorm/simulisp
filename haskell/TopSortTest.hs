{-# LANGUAGE FlexibleInstances #-}

module TopSortTest where

import Control.Monad
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


-- Automated test generation: QuickCheck --
-------------------------------------------

-- you can also use verboseCheck or other top-level testing functions
testTopSort = quickCheck (topSortCorrect :: (Graph Int) -> Bool)

-- Random directed graph generation

instance Arbitrary (Graph Int) where
  arbitrary = sized randomGraph

randomGraph :: Int -> Gen (Graph Int)
randomGraph n =
  foldM addRandomEdges (makeGraphWithNodes [1..n]) [1..n]
  where addRandomEdges graph node = do
          rand <- vectorOf n (arbitrary :: Gen Bool)
          let successors = map snd . filter fst $ zip rand [1..n]
          return $ foldl' (\g s -> addEdge g node s) graph successors
        

-- Test specification

-- Two cases:
-- * cyclic graph -> rejection
-- * acyclic graph -> returns correct linearization of partial order
topSortCorrect :: (Ord a) => Graph a -> Bool
topSortCorrect g
  | hasCycle g = topological g == Nothing
  | not (hasCycle g), Just topSort <- topological g =
      let locallyCorrect node successors =
            all (\s -> position node < position s) successors
          position (Node n) = positionMap Map.! n
          positionMap = Map.fromList $ zip topSort [1..]
      in allWithKey locallyCorrect $ g_edges_to g
  | otherwise = False
              
allWithKey :: (k -> a -> Bool) -> Map.Map k a -> Bool
allWithKey p = Map.foldr (&&) True . Map.mapWithKey p
