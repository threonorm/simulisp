module Processor.Parameters where

-- Size constants

consS, wordS, dataS, tagS, microInstrS, microAddrS :: Int

tagS  = 5
dataS = 19
wordS = tagS + dataS
consS = 2 * wordS
microInstrS = 24
microAddrS = 12
-- also: number of registers = 2^3

data Reg = Null
         | Value
         | Expr
         | Env
         | Args
         | Stack
         | Temp
         deriving (Eq, Show)

-- Enumeration of tags, and their binary format

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
         | TSync
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
         deriving (Eq, Show)

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

tagNum TCar       = 16
tagNum TCdr       = 17
tagNum TCons      = 18
tagNum TIncr      = 19
tagNum TDecr      = 20
tagNum TIsZero    = 21
tagNum TIsgt60    = 22
tagNum TIsgt24    = 23
tagNum TPrintSec  = 24
tagNum TPrintMin  = 25
tagNum TPrintHour = 26


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

type ExternalInstruction = ControlSignals Reg Bool ALUOp Immediate

data ControlSignals r s a i = CS { regRead :: r
                                 , regWrite :: r
                                 , writeReg :: s
                                 , writeTemp :: s
                                 , useGC :: s
                                 , gcOpcode :: [s]
                                 , aluCtrl :: a
                                 , loadCondReg :: s
                                 , interactWithOutside :: s
                                 , outsideOpcode :: [s]
                                 , immediate :: i
                                 }
                              deriving (Show)

                                   
data ALUOp = ALUNop | ALUIncr | ALUDecrUpper | ALUDecrImmediate
           deriving (Eq, Show)

data Immediate = ImmT Tag | ImmR ReturnTag | ImmN Int
               deriving (Eq, Show)
  
