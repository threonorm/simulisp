{-# LANGUAGE DoRec, ScopedTypeVariables #-}

import Control.Monad
import Control.Applicative

import Caillou.Circuit
import Caillou.Arithmetic
import Caillou.Patterns
import Caillou.NetlistGen
import Caillou.Simulation
import Netlist.Print
import Netlist.AST

-- main = printProgToFile (synthesizeNetlistAST (\() -> test) () []
--                                              (\ss -> zip ["s0", "s1", "s2", "s3"] ss))
--                        "counter.net"

-- test: 4-bit counter
-- test :: SimulateSeq [Bool]
-- test = do
--   wireZero <- zero
--   one4bit <- (:) <$> one <*> replicateM 3 zero
--   rec ctr <- mapM delay newctr
--       (newctr,_) <- adder (wireZero, zip one4bit ctr)
--   return ctr

-- simpler test: 2-bit counter
-- this works
test :: SimulateSeq [Bool]
test = do
  wireZero <- zero
  wireOne  <- one
  rec ctr0 <- delay newctr0
      ctr1 <- delay newctr1
      ([newctr0, newctr1],_) <- adder (wireZero, [(wireOne, ctr0), (wireZero, ctr1)])
  return [ctr0, ctr1]

test' :: SimulateSeq [Bool]
test' = do
  wireZero <- zero
  wireOne  <- one
  rec ctr <- mapMn 2 delay newctr
      (newctr,_) <- adder (wireZero, zip [wireOne, wireZero] ctr)
  return ctr

