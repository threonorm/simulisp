{-# LANGUAGE MultiParamTypeClasses, RecursiveDo #-}

module Examples where

import Control.Applicative
import Circuit

-- temporary for testing
import Arithmetic
import Simulation

orLoop :: (SequentialCircuit m s) => s -> m s
orLoop inp = do rec out <- inp -||- mem
                    mem <- delay out
                return out
                 
