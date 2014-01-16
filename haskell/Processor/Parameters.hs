module Processor.Parameters where

consS, wordS, dataS, tagS, microInstrS, microAddrS :: Int

tagS  = 5
dataS = 19
wordS = tagS + dataS
consS = 2 * wordS
microInstrS = 24
microAddrS = 12
-- also: number of registers = 2^3

-- microinstruction format: see Hardware.hs

