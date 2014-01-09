{-# LANGUAGE MultiParamTypeClasses, DoRec #-}

module Examples where

import Control.Applicative
import Control.Monad
import Circuit
import Data.List

-- temporary for testing
import Arithmetic
import Simulation

wordS = 24
addrS = 20 

orLoop :: (SequentialCircuit m s) => s -> m s
orLoop inp = do rec out <- inp -||- mem
                    mem <- delay out
                return out

bigMux a b c = zipWithM (mux3 a) b c

andAll :: (SequentialCircuit m s) => [s] -> m s
--TODO : Check the code generated
-- /!\ we can improve with a dichotomy
andAll  [a] = do return a           
andAll (t:q) = do rec out <- t -&&> andAll q
                  return out 
 
--decr :: (SequentialCircuit m s) => [s] -> m [s]
--incr :: (SequentialCircuit m s) => [s] -> m [s]
--TODO




ram :: (MemoryCircuit m s) => [s]-> s-> [s] -> [s] ->m [s]
ram addR flagW addW dataW =
    accessRAM addrS wordS (addR,flagW,addW,dataW)


goTo :: (MemoryCircuit m s) => [s] -> m [s]
goTo ptr = 
    do z <- zero
       ram ptr z [] []    

 
                 
