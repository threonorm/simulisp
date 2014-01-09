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


 
                 
