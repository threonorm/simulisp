module Digraph where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as Map 
import Data.Map (Map, (!))

data Mark = NotVisited        
          | InProgress       
          | Visited          
          deriving (Show, Eq)

data Graph a = Graph { g_nodes      :: [Node a]             
                     , g_edges_to   :: Map (Node a) [Node a]
                     , g_edges_from :: Map (Node a) [Node a]
                     } deriving (Show, Eq)
 

newtype Node a = Node { n_label :: a } deriving (Show, Eq, Ord)

mk_graph :: Graph a
mk_graph = Graph { g_nodes = []
                 , g_edges_to = Map.empty
                 , g_edges_from = Map.empty
                 }

add_nodes :: Graph a -> a -> Graph a
add_nodes g x = g { g_nodes = (Node x  : g_nodes g) }

node_for_label g x = 
 find (\n-> (n_label n) == x) (g_nodes g)

add_edge g id1 id2 = 
  let n1 = node_for_label g id1 in 
  let n2 = node_for_label g id2 in
  case (n1,n2) of --Comment faire ça proprement ? :
    (Just x, Just y) -> g{g_edges_to = Map.insert x (y:(g_edges_to g ! x)) (g_edges_to g),
                      g_edges_from = Map.insert y (x:(g_edges_from g ! y)) (g_edges_from g)}
    (_,_) -> mk_graph --Ceci n'arrive jamais

clear_marks :: (Ord a) => Graph a -> Map (Node a) Mark
clear_marks g =
  Map.fromList $ zip (g_nodes g) (repeat NotVisited)  

find_roots :: (Ord a) => Graph a -> [Node a]
find_roots g =
  filter (\n -> (g_edges_from g ! n) == []) (g_nodes g)   

has_cycle :: (Ord a) => Graph a -> Bool
has_cycle g =
  isNothing . flip runStateT initial . mapM_ visit $ g_nodes g
  where visit n = do mark <- gets (! n)
                     case mark of
                       NotVisited -> do modify $ Map.insert n InProgress
                                        mapM_ visit $ g_edges_to g ! n
                                        modify $ Map.insert n Visited   
                       InProgress -> fail "Cycle commentaire jeté"
                       Visited    -> return ()                   
        initial = clear_marks g


topological :: (Ord a) => Graph a -> Maybe [a]
topological g = case find_roots g of
    []    -> Nothing -- No roots => graph has cycle (assuming non-emptiness)
    roots -> flip evalStateT initial . foldM visit [] $ roots
  where initial = clear_marks g
        visit acc n = do
          mark <- gets (! n)
          case mark of
            NotVisited -> do modify $ Map.insert n InProgress
                             l <- foldM visit [] (g_edges_to g ! n)
                             modify $ Map.insert n Visited 
                             return ((n_label n):l++acc)
            InProgress -> fail "Cycle commentaire jeté"
            Visited -> return acc 
