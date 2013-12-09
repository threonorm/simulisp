module Lava.Combinational where

import Lava.Ref
import Lava.Generic
import Lava.Signal
import Lava.Netlist
import Lava.Sequent
import Lava.Error

import Lava.MyST
  ( ST
  , STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , runST
  )

----------------------------------------------------------------
-- simulate

simulate :: Generic b => (a -> b) -> a -> b
simulate circ inp = runST (
  do sr <- netlistST new define (struct (circ inp))
     sa <- mmap (fmap symbol . readSTRef) sr
     let res = construct sa
     return res
  )
 where
  new =
    newSTRef (wrong Lava.Error.CombinationalLoop)

  define r s =
    do s' <- mmap readSTRef s
       writeSTRef r (eval s')

----------------------------------------------------------------
-- the end.

