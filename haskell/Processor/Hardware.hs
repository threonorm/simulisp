{-# LANGUAGE MultiParamTypeClasses, DoRec #-}

module Processor.Hardware where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List

import Caillou.NetlistGen
import Caillou.Arithmetic
import Caillou.Patterns
import Caillou.Circuit

import Netlist.AST
import Netlist.Print


-- Prolegomena and specifications --
------------------------------------

-- Size constants

import Processor.Parameters

-- Control signals / microcode specification

type ControlSignals s = GenericExternalInstruction (s,s,s) s (s,s) [s]


-- TODO: implement decodeMicroInstruction AFTER the binary format has been decided
-- this must be consistent with the microassembler
                        
decodeMicroInstruction :: [s] -- EXTERNAL control signals
                              -- the 2 internal bits have already been removed
                       -> ControlSignals s
-- decodeMicroInstruction microinstr = CS rr rw wf wt md go ac ua lcr im 
--   where external = drop 2 microinstr -- first 2 bits are internal to control
--         ^ attention, ça ça devient faux
--         (rr,q0) = splitAt 3 external
--         (rw,q1) = splitAt 3 q0
--         (wf:q2) = q1
--         (wt:q3) = q2
--         (md:q4) = q3
--         (go,q5) = splitAt 2 q4
--         (ac:ua:q6) = q5 -- throwaway suffix which is useless for now
--         (lcr:im) =  q6 

-------------------------------------------------------------
-- Thomas, tu te charges de ça !!!!!
-- !!!!!!!!!!!!!!!!!!
-- un truc possiblement pertinent serait de mettre
-- decodeMicroInstruction et la fonction
-- microinstruction -> (0|1)* côte à côte dans un fichier
-- afin de pouvoir vérifier d'un coup d'oeil la cohérence
--
-- attention aussi au dispatchsuffix juste au dessous
-- note: je choisis d'utiliser les bits du suffixe pour
-- décider eval vs apply vs return dans dispatch
-- mais il faut pas que ces bits viennent influencer,
-- par exemple, l'écriture de registres
-- je les mets dans immediate parce que ça me paraît
-- inoffensif, mais si tu as mieux, n'hésites pas
-- à changer !
-------------------------------------------------------------

decodeMicroInstruction = undefined

dispatchSuffix :: [s] -- raw unformatted *external* microinstruction
               -> [s] -- 2 bits which indicate whether we should execute
                      -- eval, apply or return
dispatchSuffix = take 2 . immediate . decodeMicroInstruction



-- Strong typing for the win!

newtype TagField  s = TagField  [s]
newtype DataField s = DataField [s]
newtype Word      s = Word      [s]
newtype Cons      s = Cons      [s]

-- Is there a way to make all this repetition nicer?

muxDataField :: (Circuit m s) => s -> DataField s -> DataField s -> m (DataField s)
muxDataField c (DataField xs) (DataField ys) =
  DataField <$> bigMuxn dataS c xs ys

muxWord :: (Circuit m s) => s -> Word s -> Word s -> m (Word s)
muxWord c (Word xs) (Word ys) = Word <$> bigMuxn wordS c xs ys

muxCons :: (Circuit m s) => s -> Cons s -> Cons s -> m (Cons s)
muxCons c (Cons xs) (Cons ys) = Cons <$> bigMuxn consS c xs ys

decomposeWord :: Word s -> (TagField s, DataField s)
decomposeWord (Word w) = (TagField prefix, DataField suffix)
  where (prefix, suffix) = splitAt tagS w

recomposeWord :: TagField s -> DataField s -> Word s
recomposeWord (TagField t) (DataField d) = Word $ t ++ d

decomposeCons :: Cons s -> (Word s, Word s)
decomposeCons (Cons w) = (Word prefix, Word suffix)
  where (prefix, suffix) = splitAt wordS w

recomposeCons :: Word s -> Word s -> Cons s
recomposeCons (Word t) (Word d) = Cons $ t ++ d


-- Global view of the processor --
----------------------------------

-- Plugging together the different functional blocks and control signals

processor :: (MemoryCircuit m s) => m [s]
processor = 
  do rec controlSignals <- control condReg regOutTag

         condReg <- delay =<< mux3 (loadCondReg controlSignals,
                                    aluOverflow,
                                    regIsNil)
         let (regOutTag@(TagField tag), regOutData) = decomposeWord regOut
         regIsNil <- dichotomicFold or2 tag

         regIn <- muxWord (useGC controlSignals) aluOut gcOut

         gcOut <- memorySystem (gcOpcode controlSignals)
                               regOutData
                               regOut
                               tempOut
         (aluOut, aluOverflow) <- miniAlu controlSignals regOut
                                  
         regOut <- registerArray controlSignals regIn
         tempOut <- singleRegister (writeTemp controlSignals) regIn
         
     return (interactWithOutside controlSignals
             : outsideOpcode controlSignals)


-- Definitions for the functional blocks --
-------------------------------------------

-- Memory system (in charge of implementing alloc_cons, fetch_car, fetch_cdr)

memorySystem :: (MemoryCircuit m s) => [s] -> DataField s -> Word s -> Word s -> m (Word s)
memorySystem opCode (DataField ptr) regBus tempBus =
  do rec codeMem <- Cons <$> accessROM "rom_code" ramAddrS consS addrR
         dataMem <- Cons <$> accessRAM ramAddrS consS
                                       (addrR, allocCons, freeCounter,
                                       consRegTemp)
                             -- write to next free cell iff allocating
         freeCounter <- bigDelayn ramAddrS =<< addBitToWord (allocCons, freeCounter)
     (car, cdr) <- decomposeCons <$> muxCons codeOrData codeMem dataMem
     muxWord carOrCdr car cdr
  where [carOrCdr, allocCons] = opCode
        (codeOrData:addrR) = ptr
        (Cons consRegTemp) = recomposeCons regBus tempBus
        ramAddrS = dataS - 1 -- 1 bit reserved to choose between ROM and RAM


-- A small ALU which takes a word (i.e. tag + data) and operates on the data part;
-- it can either:
-- * increment or decrement it
-- * decrement the upper half of the word (special-purpose for lookup of local variables)
-- * subtract an immediate-mode constant (special-purpose for the clock)
-- it outputs the result of the computation + a boolean to signal overflow
-- The tag of the output is TNum unless the ALU was requested to do nothing,
-- in which case it maintains the tag of the input.
-- When computing n (input) - k (immediate) with k > n, the result is 0
                
miniAlu :: (Circuit m s) => (ControlSignals s) -> (Word s) -> m (Word s, s)
miniAlu controlSignals inputWord = do
  let (TagField tag, DataField df) = decomposeWord inputWord
      upperS = dataS `div` 2
      lowerS = dataS - upperS
      (ctrl0, ctrl1) = aluCtrl controlSignals
      imm = immediate controlSignals
      
  -- ensure consistency with encodeALUOp in Parameters.hs!
  let actOnLower = ctrl0 -- else, don't touch the lower half
      decr       = ctrl1
  doSomething <- ctrl0 -||- ctrl1
  decrImm <- ctrl0 -&&- ctrl1

  -- Expected specification for the second operand and initial carry
  -- TODO: test with quickcheck
  -- Abbreviations: so(l|u) = second operand (lower|upper) half
  -- 
  -- Operation | sol       | sou           | initial carry
  -- -----------------------------------------------------
  -- Nop       | zero      | zero          | 0
  -- Incr      | zero      | zero          | 1
  -- DecrUpper | 11...11   | zero          | 0
  -- DecrImm   | 11...11   | 11..not(imm)  | 1

  solImm <- mapMn immediateS (\i -> decrImm -&&> neg i) imm
  let sol = replicate (lowerS - immediateS) decrImm ++ solImm
      sou = replicate upperS decr
      secondOperand = sol ++ sou
      initialCarry = actOnLower

  (result, finalCarry) <- adder (initialCarry, zip df secondOperand)
  overflowBit <- decr -^^- finalCarry
  
  tagOut <- bigMuxnWithConst tagS doSomething tag (tagBin TNum)
  
  return (recomposeWord (TagField tagOut) (DataField result),
          overflowBit)

  where bigMuxnWithConst 0 _ _ _ = return []
        bigMuxnWithConst n ctrl ~(s:ss) ~(b:bs)
          -- if ctrl then true  else s
          | b         = (:) <$> (ctrl -||- s)
                            <*> bigMuxnWithConst n ctrl ss bs
          -- if ctrl then false else s
          | otherwise = (:) <$> (neg ctrl <&&- s)
                            <*> bigMuxnWithConst n ctrl ss bs
                        

-- Registers

-- The array of registers

registerArray :: (MemoryCircuit m s) => ControlSignals s -> Word s -> m (Word s)
registerArray controlSignals (Word regIn) =
  Word <$> accessRAM 3 wordS ([r0,r1,r2], w, [w0,w1,w2], regIn)
  where (r0, r1, r2) = regRead  controlSignals
        (w0, w1, w2) = regWrite controlSignals
        w = writeReg controlSignals
  
-- A single register (used for temp)

singleRegister :: (SequentialCircuit m s) => s -> Word s -> m (Word s)
singleRegister writeEnable (Word input) = do
  rec delays <- mapMn wordS delay newValue
      newValue <- zipWithMn wordS (\inp del -> mux3 (writeEnable, del, inp))
                                  input delays
  return $ Word delays
  

-- The control logic
-- The microprogram is stored in a ROM, the function of the hardware
-- plumbing here is to handle state transitions

control :: (MemoryCircuit m s) => s -> TagField s -> m (ControlSignals s)
control cond (TagField tag) =
  do wireZero <- zero

     rec mpc <- bigDelayn microAddrS newMpc -- microprogram counter
         microInstruction <- accessROM "rom_microcode"
                                       microAddrS microInstrS mpc

         let (jump:choice:external) = microInstruction
         let dispatch    = choice -- choice between normal (0) and dispatch (1)
         let conditional = choice -- choice between jump and branch

         normalNextAddr <- incrementer mpc
         let initialStateForTag = replicate (microAddrS - tagS - 2) wireZero
                                  ++ tag
                                  ++ dispatchSuffix external

         jumpOK <- neg conditional <||- cond
         nonLocal <- mux3 (jump, dispatch, jumpOK)
         
         newMpcNonLocal <- bigMuxn microAddrS jump
                                   initialStateForTag
                                   (take microAddrS external)
           
         newMpc <- bigMuxn microAddrS nonLocal
                           normalNextAddr
                           newMpcNonLocal                           

     notJump <- neg jump
     neutralized <- mapM (notJump -&&-) external
     return $ decodeMicroInstruction neutralized


-- Output netlist
-- TODO: improve and put in another file
main :: IO ()
main = do
  let inp = ()
      circ () = processor
      (_,nl,_) = synthesizeBarebonesNetlist circ inp
  writeFile "foobar.net" . unlines . map show $ nl


