module Processor.Parameters where

import Lisp.SCode (Tag(..))
import Lisp.Primitives

import Caillou.Circuit

-- Size constants

consS, wordS, dataS, tagS, microInstrS, microAddrS, immediateS, upperS, lowerS :: Int

tagS  = 5
dataS = 19
wordS = tagS + dataS
upperS = dataS `div` 2
lowerS = dataS - upperS
consS = 2 * wordS
microInstrS = 24
microAddrS = 12
immediateS = 6 -- should not exceed dataS / 2, cf. ALU implementation
-- also: number of registers = 2^3


data Reg = Null
         | Value
         | Expr
         | Env
         | Args
         | Stack
         | Temp
         deriving (Eq, Show)

-- Enumeration of the registers
regNum :: Reg -> Int
regNum Null  = 0
regNum Value = 1
regNum Expr  = 2
regNum Env   = 3
regNum Args  = 4
regNum Stack = 5
regNum Temp  = 6


-- Enumeration of the tags
-- For the definition of the tags, see Lisp.SCode
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
tagNum TSync     = 14

tagNum (TPrim PCar      ) = 16
tagNum (TPrim PCdr      ) = 17
tagNum (TPrim PCons     ) = 18
tagNum (TPrim PIncr     ) = 19
tagNum (TPrim PDecr     ) = 20
tagNum (TPrim PIsZero   ) = 21
tagNum (TPrim PIsgt60   ) = 22
tagNum (TPrim PIsgt24   ) = 23
tagNum (TPrim PPrintSec ) = 24
tagNum (TPrim PPrintMin ) = 25
tagNum (TPrim PPrintHour) = 26


-- 00 suffix: bootloading segment
tagSuffixS :: Int
tagSuffixS = 2

evalSuffix, applySuffix, returnSuffix :: [Bool]
evalSuffix   = [True , False]
applySuffix  = [False, True]
returnSuffix = [True , True]

data ReturnTag = RFirst
               | RNext
               | RLast
               | RApplyOne           
               | RLet
               | RSequence
               | RApply
               deriving (Eq, Show)

returnNum :: ReturnTag -> Int
returnNum RFirst    = 0
returnNum RNext     = 1
returnNum RLast     = 2
returnNum RApplyOne = 3
returnNum RLet      = 4
returnNum RSequence = 5
returnNum RApply    = 6

tagBinGeneric :: Int -> [Bool]
tagBinGeneric = go tagS
  where go 0 _ = []
        go k n = (n `mod` 2 /= 0) : go (k-1) (n `div` 2)

tagBin    = tagBinGeneric . tagNum
returnBin = tagBinGeneric . returnNum

-- microinstruction format

-- s: signal type (polymorphic to use with Caillou)
data MicroInstruction = ExtInstr ExternalInstruction
                      | Jump { jumpIsConditional :: Bool
                             , jumpAddress :: Int }
                      | Dispatch Reg [Bool]
                      deriving (Show)

type ExternalInstruction = GenericExternalInstruction Reg Bool ALUOp GCOp Immediate

data GenericExternalInstruction r s a g i =
  CS { regRead :: r
     , regWrite :: r
     , writeReg :: s
     , writeTemp :: s
     , useGC :: s
     , gcOpcode :: g
     , aluCtrl :: a
     , loadCondReg :: s
     , interactWithOutside :: s
     , outsideOpcode :: [s]
     , immediate :: i
     }
  deriving (Show)

                                   
data ALUOp = ALUNop | ALUIncr | ALUDecrUpper | ALUDecrImmediate
           deriving (Eq, Show)

-- This is actually a smart encoding
-- first bit means "do we act on the lower half?"
-- second bit means "are we subtracting something?"
encodeALUOp :: ALUOp -> (Bool, Bool)
encodeALUOp ALUNop            = (False, False)
encodeALUOp ALUIncr           = (True,  False)
encodeALUOp ALUDecrUpper      = (False, True)
encodeALUOp ALUDecrImmediate  = (True,  True)


data GCOp = GCNop | GCAlloc | GCFetchCar | GCFetchCdr
           deriving (Eq, Show)

encodeGCOp :: GCOp -> (Bool, Bool)
encodeGCOp GCNop       = (False, False)
encodeGCOp GCFetchCar  = (True,  False)
encodeGCOp GCFetchCdr  = (False, True)
encodeGCOp GCAlloc     = (True,  True)
                    
data Immediate = ImmT Tag | ImmR ReturnTag | ImmN Int
               deriving (Eq, Show)


-- Binary serialization of microinstructions


-- Binary format spec
-- ------------------
--
-- Signals stored in ROM: (total: 24 bits)
--
-- Name of field    | # of bits |
--  internalControl |        2  |  2
--  regRead         |        3  |  5 
--  regWrite        |        3  |  8   
--  writeReg        |        1  |  9
--  writeTemp       |        1  | 10
--  gcOpcode        |        2  | 12
--  aluCtrl         |        2  | 14
--  loadCondReg     |        1  | 15
--  outsideOpcode   |        3  | 18
--  immediate       |        6  | 24
--
-- Derived signals: useGC (1 bit) and interactWithOutside (1 bit)

type ControlSignals s = GenericExternalInstruction
                        (s,s,s) -- registers on 3 bits
                        s       -- signal type
                        (s,s)   -- aluCtrl on 2 bits
                        (s,s)   -- gcOpcode on 2 bits
                        [s]     -- immediate = list


-- Takes EXTERNAL part of microinstruction from the ROM
-- computes derived signals
-- and returns the signals structured in a record
decodeMicroInstruction :: (Circuit m s) => [s] -> m (ControlSignals s)
decodeMicroInstruction extMicroInstr = do
  ug  <- go1 -||- go2
  iwo <- (oo1 -||- oo2) <||- oo3
  return $ CS (rr1, rr2, rr3) (rw1, rw2, rw3) wr wt ug (go1, go2) (ac1, ac2) lcr
              iwo [oo1, oo2, oo3] imm
  where (rr1:rr2:rr3
         :rw1:rw2:rw3
         :wr
         :wt
         :go1:go2
         :ac1:ac2
         :lcr
         :oo1:oo2:oo3
         :imm) = extMicroInstr

 

                        
  
