module Processor.Parameters where

consS, wordS, dataS, tagS, microInstrS, microAddrS :: Int

tagS  = 5
dataS = 19
wordS = tagS + dataS
consS = 2 * wordS
microInstrS = 24
microAddrS = 12
-- also: number of registers = 2^3

data Tag = TNil
         | TLocal
         | TGlobal
         | TClosure
         | TCond
         | TList
         | TNum
         | TProc
         | TFirst
         | TNext
         | TLast
         | TApplyOne
         | TLet
         | TSequence
         -- Primitives
         | TCar
         | TCdr 
         | TCons
         | TIncr
         | TDecr
         | TIsZero 
         | TIsgt60
         | TIsgt24
         | TPrintSec
         | TPrintMin
         | TPrintHour

-- Explicit instead of "deriving Enum" for easy reference
tagNum :: Tag -> Int
tagNum TNil      =  0
tagNum TLocal    =  1
tagNum TGlobal   =  2
tagNum TClosure  =  3
tagNum TCond     =  4
tagNum TList     =  5
tagNum TNum      =  6
tagNum TProc     =  7
tagNum TFirst    =  8
tagNum TNext     =  9
tagNum TLast     = 10
tagNum TApplyOne = 11
tagNum TLet      = 12
tagNum TSequence = 13

tagBin :: Tag -> [Bool]
tagBin = go tagS . tagNum
  where go 0 _ = []
        go k n = (n `mod` 2 /= 0) : go (k-1) (n `div` 2)


data ReturnTag = RFirst
               | RNext
               | RLast
               | RApplyOne           
               | RLet
               | RSequence
               | RApply
                 

-- microinstruction format: see Hardware.hs

