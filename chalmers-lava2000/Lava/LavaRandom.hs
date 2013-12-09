module Lava.LavaRandom
  ( Rnd
  , newRnd
  , next
  , split
  )
 where

import System.Random
  ( StdGen
  , newStdGen
  , next
  , split
  )

type Rnd = StdGen

newRnd = newStdGen
