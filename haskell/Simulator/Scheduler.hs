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
import NetlistParser
import qualified Digraph as G

readExp :: Exp -> [Ident]
readExp expr = args2deps $ case expr of
  Earg x -> [x] 
  Ereg _   -> [] -- a register outputs its input at cycle (n-1):
                 -- no dependency on input at cycle n
  Enot x -> [x]
  Ebinop _ x y -> [x, y]
  Emux x y z -> [x, y, z]   
  Erom _ _ x -> [x]
  Eram _ _ x _ _ _ -> [x] -- RAM behaves like a register:
                          -- the only dependency is the memory address to read
  Econcat x y -> [x, y]
  Eslice _ _ x -> [x]
  Eselect _ x -> [x]
  where args2deps = nub . mapMaybe f
        f (Avar ident) = Just ident
        f (Aconst _  ) = Nothing

schedule :: Program -> Maybe Program -- error = combinatorial cycle
schedule prog = f <$> G.topological depGraph
  where eqs = p_eqs prog
        depGraph = makeEdges . G.makeGraphWithNodes . nub
                   $ p_inputs prog
                   ++ (map fst . Map.toList $ p_vars prog)
                   ++ p_outputs prog 
        makeEdges g = foldl' addDeps g eqs
        addDeps g (label, expr) = foldl' (\acc x -> G.addEdge acc x label) g
                                  $ readExp expr
        eqMap = Map.fromList $ map (\(a,b) -> (a, (a,b))) eqs
        f toposort = prog { p_eqs =  mapMaybe (flip Map.lookup eqMap) toposort }
