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
microInstrS = 13
microAddrS = 8
-- also : number of registers = 2^3

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
  do rec -- wireZero <- zero
         -- wireOne <- one
         controlSignals  <- control regOut
         regIn <- bigMux (muxData controlSignals) regOut gcOut  
         gcOut <- memorySystem (gcOpcode controlSignals)
                               regOut
                               temporaries
         temporaries <- accessRAM 0 wordS ([],
                                           writeTemp controlSignals,
                                           [],
                                           regIn)
         miniAlu <- incrOrDecr (aluCtrl controlSignals !! 0) (drop tagS gcOut)
         regOut <- bigMux (aluCtrl controlSignals !! 1) miniAlu
                   =<< registerArray controlSignals regIn
     return []


memorySystem :: (MemoryCircuit m s) => [s] -> [s] -> [s] -> m [s]  
memorySystem opCode regOut temporary=
  do rec wireZero <- zero
         let alloc = opCode !! 1
         newcounter <- mapM delay =<< fst <$> adder (alloc, zip (repeat wireZero)  newcounter)   
         programMem <- accessROM (dataS-1) (2*wordS) $ drop (tagS+1) regOut
         dataMem <- ram (drop (tagS+1) regOut) alloc newcounter (regOut++temporary)
         cons <- bigMux (regOut!!tagS) programMem dataMem 
     let car = take wordS cons
         cdr = drop wordS cons
     bigMux (head opCode) car cdr  



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


ram :: (MemoryCircuit m s) => [s] -> s -> [s] -> [s] -> m [s]
ram addR flagW addW dataW = 
    accessRAM dataS (2*wordS) (addR,flagW,addW,dataW)


registerArray :: (MemoryCircuit m s) => ControlSignals s -> [s] -> m [s]
registerArray controlSignals regIn = 
    accessRAM 3 wordS (regRead   controlSignals,
                       writeFlag controlSignals,
                       regWrite  controlSignals,
                       regIn) 

control :: (MemoryCircuit m s) =>  [s] ->  m (ControlSignals s)
control regOut = 
  do rec internIncr <- incrementer mpc
         -- maybe replace by a mux between 3 alternatives ?
         newMpc <- bigDoubleMux (take 2 microInstruction)
                                internIncr
                                (take microAddrS . drop 2 $ microInstruction)
                                regOut
                                regOut
         mpc <- mapM delay newMpc
         microInstruction <- accessROM microAddrS microInstrS mpc 
     return $ decodeMicroInstruction microInstruction

