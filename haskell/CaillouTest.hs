{-# LANGUAGE DoRec #-}

import Control.Monad
import Control.Applicative

import Caillou.Circuit
import Caillou.Arithmetic
import Caillou.Patterns
import Caillou.NetlistGen
import Caillou.Simulation
import Netlist.Print
import Netlist.AST

main = printProgToFile (synthesizeNetlistAST (\() -> clockProc) () []
                                             (\out -> [ ("out" ++ show i, o)
                                                      | (i,o) <- zip [0..] out ]))
                       "clock_proc.net"

clockProc = do
  wireZero <- zero
  wireOne  <- one
  rec ctr2 <- mapMn 2 delay newctr2
      (newctr2,_) <- adder (wireZero, zip [wireOne, wireZero] ctr2)
  rec ctr4 <- mapMn 4 delay newctr4
      (newctr4,_) <- adder (wireZero, zip (wireOne:replicate 3 wireZero) ctr4)
  return $ [wireOne] ++ ctr2 ++ ctr4
  

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

