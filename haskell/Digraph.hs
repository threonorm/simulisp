module Digraph where

import Prelude hiding (notElem, mapM_) -- hide monomorphic definitions
                                       -- conflicting with Data.Foldable
import Control.Monad.State hiding (mapM_)
import Control.Applicative
import Data.Maybe
import Data.Foldable
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

emptyGraph :: Graph a
emptyGraph = Graph { g_nodes = []
                   , g_edges_to = Map.empty  
                   , g_edges_from = Map.empty
                   }
             
makeGraphWithNodes :: (Ord a) => [a] -> Graph a
makeGraphWithNodes = foldl' add_node emptyGraph

add_node :: (Ord a) => Graph a -> a -> Graph a
add_node g x = g { g_nodes = Node x : g_nodes g
                 , g_edges_to   = Map.insert (Node x) [] $ g_edges_to g
                 , g_edges_from = Map.insert (Node x) [] $ g_edges_from g
                 } -- need to add empty adjacency lists to avoid
                   -- "key not found" problems later

add_edge :: (Ord a) => Graph a -> a -> a -> Graph a
add_edge g id1 id2 =
  g { g_edges_to   = Map.adjust (n2:) n1 $ g_edges_to g
    , g_edges_from = Map.adjust (n1:) n2 $ g_edges_from g
    }
  where n1 = Node id1
        n2 = Node id2

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
topological g = do
  (result, finalMarks) <- flip runStateT (clear_marks g) . foldM visit []
                          $ find_roots g
  guard . notElem NotVisited $ finalMarks
  return result
  where visit acc n = do
          mark <- gets (! n)
          case mark of
            NotVisited -> do modify $ Map.insert n InProgress
                             l <- foldM visit [] (g_edges_to g ! n)
                             modify $ Map.insert n Visited 
                             return ((n_label n):l++acc)
            InProgress -> fail "Cycle commentaire jeté"
            Visited -> return acc 
