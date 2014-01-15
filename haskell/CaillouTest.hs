{-# LANGUAGE DoRec #-}

import Control.Monad
import Control.Applicative

import Caillou.Circuit
import Caillou.Arithmetic
import Caillou.Patterns
import Caillou.NetlistGen
import Netlist.Print
import Netlist.AST

main = printProgToFile (synthesizeNetlistAST (\() -> test) () []
                                             (\ss -> zip ["s0", "s1", "s2", "s3"] ss))
                       "counter.net"

-- test: 4-bit counter
test = do
  wireZero <- zero
  one4bit <- (:) <$> one <*> replicateM 3 zero
  rec ctr <- mapM delay newctr
      (newctr,_) <- adder (wireZero, zip one4bit ctr)
  return ctr

