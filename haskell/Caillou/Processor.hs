{-# LANGUAGE MultiParamTypeClasses, DoRec #-}

module Processor where

import Control.Applicative
import Control.Monad
import Circuit
import Data.List

import Arithmetic
import Patterns

-- Size constants

wordS, dataS, tagS, microInstrS, microAddrS :: Int

tagS  = 4
dataS = 20
wordS = tagS + dataS
cellS = 2 * wordS
microInstrS = 16
microAddrS = 8
-- also: number of registers = 2^3

-- Word handling (with some strong typing)
-- idea: use strong typing to distinguish between words and cons cells as well

newtype TagField  s = TagField  [s]
newtype DataField s = DataField [s]

decomposeWord :: [s] -> (TagField s, DataField s)
decomposeWord word = (TagField prefix, DataField suffix)
  where (prefix, suffix) = splitAt tagS word

-- Control signals / microcode specification

data ControlSignals s = CS { regRead   :: [s] -- 3 bits
                           , regWrite  :: [s] -- 3 bits
                           , writeFlag :: s   -- 1 bit
                           , writeTemp :: s   -- 1 bit
                           , muxData   :: s   -- 1 bit
                           , gcOpcode  :: [s] -- 2 bits
                           , aluCtrl   :: [s] -- 2 bits
                           } --           Total: 13 bits (=microInstrS)
                        
decodeMicroInstruction :: [s] -> ControlSignals s
decodeMicroInstruction microinstr = CS rr rw wf wt md go ac
  where external = drop 2 microinstr -- first 2 bits are internal to control
        (rr,q0) = splitAt 3 external
        (rw,q1) = splitAt 3 q0
        (wf:q2) = q1
        (wt:q3) = q2
        (md:q4) = q3
        (go,ac) = splitAt 2 q4

processor :: (MemoryCircuit m s) => m [s]
processor = 
  do rec controlSignals <- control regOutTag
         regIn <- bigMux (muxData controlSignals) regOut gcOut  
         gcOut <- memorySystem (gcOpcode controlSignals)
                               regOutData
                               regOut
                               tempOut
         tempOut <- accessRAM 0 wordS ([],
                                       writeTemp controlSignals,
                                       [],
                                       regIn)
         miniAlu <- incrOrDecr (aluCtrl controlSignals !! 0) (drop tagS gcOut)
         regOut <- bigMux (aluCtrl controlSignals !! 1) miniAlu
                   =<< registerArray controlSignals regIn
         let (regOutTag, regOutData) = decomposeWord regOut
     return []


memorySystem :: (MemoryCircuit m s) => [s] -> DataField s -> [s] -> [s] -> m [s]  
memorySystem opCode (DataField ptr) regOut tempOut =
  do rec codeMem <- accessROM (dataS-1) cellS addrR
         dataMem <- accessRAM (dataS-1) cellS -- write to next free cell iff allocating
                              (addrR, allocCons, freeCounter, regOut ++ tempOut)
         freeCounter <- mapM delay =<< addBitToWord (allocCons, freeCounter)
     (car, cdr) <- splitAt wordS <$> bigMux codeOrData codeMem dataMem 
     bigMux carOrCdr car cdr
  where [carOrCdr, allocCons] = opCode
        (codeOrData:addrR) = ptr



orAll :: (SequentialCircuit m s) => [s] -> m s
--TODO : Check the code generated
-- /!\ we can improve with a dichotomy
orAll  [a] = do return a           
orAll (t:q) = do out <- t -||> orAll q
                 return out 
 
incrOrDecr :: (SequentialCircuit m s) => s -> [s] -> m [s]
incrOrDecr bitChoice (t:q) =
  do wireOne <- one
     wireZero <- zero
     fst <$> adder (wireZero, (wireOne,t):zip (repeat bitChoice) q)


registerArray :: (MemoryCircuit m s) => ControlSignals s -> [s] -> m [s]
registerArray controlSignals regIn = 
    accessRAM 3 wordS (regRead   controlSignals,
                       writeFlag controlSignals,
                       regWrite  controlSignals,
                       regIn) 

control :: (MemoryCircuit m s) => TagField s ->  m (ControlSignals s)
control (TagField regOutTag) = 
  do rec microInstruction <- accessROM microAddrS microInstrS mpc
         let (jump:dispatchOnTag:_) = microInstruction
         internIncr <- incrementer mpc
         -- maybe replace by a mux between 3 alternatives ?
         newMpc <- bigDoubleMux (take 2 microInstruction)
                                internIncr
                                (take microAddrS . drop 2 $ microInstruction)
                                regOutTag
                                regOutTag
         mpc <- mapM delay newMpc
     return $ decodeMicroInstruction microInstruction

