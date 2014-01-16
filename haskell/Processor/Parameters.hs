module Processor.Parameters where

consS, wordS, dataS, tagS, microInstrS, microAddrS :: Int

tagS  = 4
dataS = 20
wordS = tagS + dataS
consS = 2 * wordS
microInstrS = 16
microAddrS = 12
-- also: number of registers = 2^3

-- microinstruction format: see Hardware.hs

