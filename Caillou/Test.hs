module Lava.Test where

import Lava.Signal
import Lava.Sequential
import Lava.Generic

import Lava.LavaRandom
  ( newRnd
  )

----------------------------------------------------------------
-- test

test :: (Constructive a, Show b, Generic b) => (a -> b) -> IO [b]
test circ =
  do rnd <- newRnd
     let res = simulateSeq (\_ -> circ (random rnd)) (replicate 100 ())
     print res
     return res

----------------------------------------------------------------
-- the end.

