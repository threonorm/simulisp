module Scheduler where

import Prelude hiding (notElem, mapM_)
import Control.Monad.State hiding (mapM_)
import Control.Applicative
import Data.Maybe
import Data.Foldable
import Data.List (nub) 
import qualified Data.Map as Map
import Data.Map (Map, (!))

import NetlistAST
import qualified Digraph as G

under_arg arg =
  case arg of 
    Avar ident -> Just ident
    Aconst value -> Nothing

read_exp exp = 
  case exp of
         Earg arg -> maybeToList $ under_arg arg 
         Ereg ident -> [ident] 
         Enot arg-> maybeToList $ under_arg arg 
         Ebinop binop arg1 arg2 ->nub $ mapMaybe under_arg [arg1, arg2]
         Emux arg1 arg2 arg3-> nub $ mapMaybe under_arg [arg1, arg2, arg3]   
         Erom _ _ arg -> maybeToList $ under_arg arg
         Eram _ _ arg1 arg2 arg3 arg4-> nub $  mapMaybe under_arg [arg1, arg2, arg3, arg4] 
         Econcat arg1 arg2 -> nub $ mapMaybe under_arg [arg1, arg2]
         Eslice int1 int2 arg -> maybeToList $ under_arg arg
         Eselect int arg -> maybeToList $ under_arg arg


schedule :: Program -> Maybe Program -- error = combinatorial cycle
schedule prog = f <$> G.topological depGraph
  where eqs = p_eqs prog
        depGraph = makeEdges . G.makeGraphWithNodes $ map fst eqs
        makeEdges g = foldl' addDeps g eqs
        addDeps g (label, expr) = foldl' (\acc x -> G.add_edge acc x label) g
                                  $ read_exp expr
        eqMap = Map.fromList $ map (\(a,b) -> (a, (a,b))) eqs
        f toposort = prog { p_eqs = (eqMap !) <$> toposort }
