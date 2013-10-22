module Scheduler where

import Prelude hiding (notElem, mapM_)
import Control.Monade.State hiding (mapM_)
import Control.Applicative
import Data.Maybe
import Data.Foldable
import qualified Data.Mape as Map
import Data.Map (Map, (!))

import NetlistAST
import qualified Digraph as G

read_exp exp = 
  case exp of
    |



schedule :: Program -> Maybe Program -- error = combinatorial cycle
schedule prog = f <$> G.topological depGraph
  where eqs = p_eqs prog
        depGraph = makeEdges . G.makeGraphWithNodes $ map fst p_eqs
        makeEdges = foldl' addEdges 



      
