{-# LANGUAGE MultiParamTypeClasses, DoRec #-}

module Processor where

import Control.Applicative
import Control.Monad
import Circuit
import Data.List

import Arithmetic
import Patterns


-- Prolegomena and specifications --
------------------------------------

-- Size constants

cellS, wordS, dataS, tagS, microInstrS, microAddrS :: Int

tagS  = 4
dataS = 20
wordS = tagS + dataS
cellS = 2 * wordS
microInstrS = 16
microAddrS = 12
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
                           , aluCtrl   :: s   -- 1 bit
                           , useAlu    :: s   -- 1 bit
                           } --           Total: 13 bits (=microInstrS)
                        
decodeMicroInstruction :: [s] -> ControlSignals s
decodeMicroInstruction microinstr = CS rr rw wf wt md go ac ua
  where external = drop 2 microinstr -- first 2 bits are internal to control
        (rr,q0) = splitAt 3 external
        (rw,q1) = splitAt 3 q0
        (wf:q2) = q1
        (wt:q3) = q2
        (md:q4) = q3
        (go,q5) = splitAt 2 q4
        [ac,ua] = q5


-- Global view of the processor --
----------------------------------

-- Plugging together the different functional blocks and control signals

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
         alu <- miniAlu (aluCtrl controlSignals) (drop tagS gcOut)
         regOut <- bigMux (useAlu controlSignals) alu
                   =<< registerArray controlSignals regIn
         let (regOutTag, regOutData) = decomposeWord regOut
     return []


-- Definitions for the functional blocks --
-------------------------------------------

-- Memory system (in charge of implementing alloc_cons, fetch_car, fetch_cdr)
-- Implementation 

memorySystem :: (MemoryCircuit m s) => [s] -> DataField s -> [s] -> [s] -> m [s]  
memorySystem opCode (DataField ptr) regOut tempOut =
  do rec codeMem <- accessROM (dataS-1) cellS addrR
         dataMem <- accessRAM (dataS-1) cellS -- write to next free cell iff allocating
                              (addrR, allocCons, freeCounter, regOut ++ tempOut)
         freeCounter <- bigDelay =<< addBitToWord (allocCons, freeCounter)
     (car, cdr) <- splitAt wordS <$> bigMux codeOrData codeMem dataMem 
     bigMux carOrCdr car cdr
  where [carOrCdr, allocCons] = opCode
        (codeOrData:addrR) = ptr


-- Testing equality to zero

isZero :: (Circuit m s) => [s] -> m s
isZero = neg <=< orAll
  --TODO : Check the code generated
  -- /!\ we can improve with a dichotomy
  where orAll  [a] = return a           
        orAll (t:q) = do out <- t -||> orAll q
                         return out 


-- A small ALU which can either increment or decrement its argument
        
miniAlu :: (SequentialCircuit m s) => s -> [s] -> m [s]
miniAlu incrOrDecr (t:q) =
  do wireOne <- one
     wireZero <- zero
     fst <$> adder (wireZero, (wireOne,t):zip (repeat incrOrDecr) q)


-- The array of registers

registerArray :: (MemoryCircuit m s) => ControlSignals s -> [s] -> m [s]
registerArray controlSignals regIn = 
    accessRAM 3 wordS (regRead   controlSignals,
                       writeFlag controlSignals,
                       regWrite  controlSignals,
                       regIn) 

-- The control logic
-- The microprogram is stored in a ROM, the function of the hardware
-- plumbing here is to handle state transitions

control :: (MemoryCircuit m s) => TagField s ->  m (ControlSignals s)
control (TagField tag) =
  do wireZero <- zero
     let initialStateForTag = [wireZero] ++ tag
                              ++ replicate (microAddrS - tagS - 1) wireZero
     rec microInstruction <- accessROM microAddrS microInstrS mpc
         let (jump:dispatchOnTag:suffix) = microInstruction
         nextAddr <- incrementer mpc
         -- maybe replace by a mux between 3 alternatives ?
         mpc <- bigDelay newMpc
         newMpc <- bigDoubleMux [jump, dispatchOnTag]
                                nextAddr
                                (take microAddrS suffix)
                                initialStateForTag
                                initialStateForTag
     notJump <- neg jump
     neutralized <- mapM (notJump -&&-) microInstruction
     return $ decodeMicroInstruction neutralized

