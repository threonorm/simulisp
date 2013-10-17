-- Search if a graph contains a cycle
import Control.Monad.Error
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as Map 
import Data.Map (Map)

data Mark =
  NotVisited
 | InProgress
 | Visited
 deriving (Show, Eq)

data Graph a =
 Graph { g_nodes :: [Node a],
         g_edges_to :: Map (Node a) [Node a],
         g_edges_from :: Map (Node a) [Node a]} deriving (Show,Eq)



newtype Node a = 
  Node { n_label :: a} deriving (Show,Eq)


--Frozen
mk_graph = Graph{g_nodes=[],
                 g_edges_to = Map.empty,
                 g_edges_from = Map.empty,
                 }--g_visited = Map.empty  } 

--Return the new graph 
add_nodes g x =
 let n = Node {n_label = x} in
    g{ g_nodes = (n:g_nodes g)}
       --g_visited = Map.insert n NotVisited (g_visited g)} 

node_for_label g x = 
 find (\n-> (n_label n) == x) (g_nodes g)

add_edge g id1 id2 = 
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  g{g_edges_to = Map.insert n1 (n2:(g_edges_to g Map.! n1)) (g_edges_to g),
    g_edges_from = Map.insert n2 (n1:(g_edges_from g Map.! n2)) (g_edges_from g)}

clear_marks g =
  Map.fromList $ zip (g_nodes g) (repeat NotVisited)  
 
find_roots g =
  filter (\n -> (g_edges_from g  Map.! n) ==[]) (g_nodes g)   

has_cycle g =
  isNothing . runState initial . runMaybeT . mapM_ visit $ g_nodes g
  where visit n = case g_visited g Map.! n of
                      NotVisited -> do modify $ Map.insert n InProgress
                                       mapM_ visit $ g_edges_to g Map.! n
                                       modify $ Map.insert n Visited   
                      InProgress -> fail "Cycle commentaire jeté"
                      Visited    -> return ()                   
      

  
