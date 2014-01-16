module Assembler.Assembler where

import Data.List
import Processor.Parameters

--------- MiniAssembler V0.1 -------------


data Instruction =
     ExtI ExternalInstruction
    | IntI InternalInstruction
    | Dispatch Reg
    | Label (String,Int)        --Int for alignment purpose : in number of words 


--There are three types of instructions : Labels, classical instructions using
--the mini ALU and finishing by the incrementation of the micro program counter,
--and jumps (conditional or not).

data ExternalInstruction = 
    C { regRead :: Reg,
      regWrite :: Reg,
      writeFlag :: Bool,
      writeTemp :: Bool,
      muxData :: Bool,
      gcOpcode :: (Bool,Bool),
      aluCtrl :: Bool,
      useAlu :: Bool,
      immediate :: [Bool],
      loadCondReg :: Bool}

data InternalInstruction =
    D {isCond :: Bool,
     addr :: Label
    }

type Label =  String


data Reg = Value
    | Expr
    | Env
    | Args
    | Stack          
    | Temp

type Intermediate =  Either String Instruction 

--MiniAssembler makes two passes

assemble :: [Instruction] -> Maybe String 
assemble code =
  if checkPosition inter 0 
  then Just $ assembleSecond inter 0 inter 
  else Nothing
  where inter =  map assembleFirst code


assembleFirst :: Instruction -> Intermediate
assembleFirst (ExtI instr) = Left ("0"++                -- Bit of internal incr 
 (regToString . regRead $  instr ) ++
 (regToString . regWrite $  instr ) ++
 (printBool . writeFlag $  instr ) ++
 (printBool . writeTemp $  instr ) ++
 (printBool . muxData $  instr ) ++
 (printBool . fst . gcOpcode $  instr ) ++
 (printBool . snd . gcOpcode $ instr ) ++
 (printBool . aluCtrl $ instr ) ++
 (printBool . useAlu $ instr )++
 replicate (microInstrS-15) '0')  --Padding immediate + lgr

assembleFirst (Dispatch reg)=
  Left ( "1" ++
         regToString reg ++
         replicate 19 '0') -- Padding with 0 
assembleFirst (IntI instr) = 
  Right (IntI instr) 
assembleFirst (Label blabla) =
  Right (Label blabla)



assembleSecond :: [Intermediate] -> Int -> [Intermediate] -> String  -- Code,position, list from position, label
assembleSecond code pos [] = []
assembleSecond code pos ((Left blabla):q) = "0"++blabla++assembleSecond code (pos+1) q

assembleSecond code pos ((Right(IntI instr)): q) =  
 ("1"++ -- Bit of Jump
  (printBool . isCond $ instr) ++
  ( printAddr 12 . position code. addr $ instr) ++
    replicate (microInstrS - microAddrS - 2) '0'  --Useless bit in this case
 ) ++ assembleSecond code (pos+1) q
assembleSecond code pos ((Right(Label (s,t))): q) = 
  (take (microInstrS *floodSize) $ repeat '0') ++ assembleSecond code t q
  where floodSize = t-pos 



--Give the position of the declaration of the Label in the Code (in words).
position :: [Intermediate] -> Label -> Int
position ((Right (Label (s,t))):q) lab =
 if s==lab then t else position q lab
position (_:q) lab =  position q lab


--To check if the assembly is possible with the alignment required by the user.
checkPosition :: [Intermediate] -> Int -> Bool 
checkPosition [] prec = True  
checkPosition (Right(Label (s,t)):q) prec = if prec <= t then checkPosition q t 
             else False
checkPosition (_:q) prec = checkPosition q $ prec + 1  --In number of words

---------- MiniASM as a sort of an eDSL  ---------

(^=) :: Reg -> Reg -> Instruction
reg1 ^= reg2 =  ExtI ground{regRead =  reg2,
                       regWrite = reg1,
                       writeFlag = True 
                       }

jmp :: Label ->  Instruction 
jmp label = IntI $ D {isCond = False,
                      addr = label  
                    }

moveToTemp :: Reg -> Instruction
moveToTemp regread = ExtI $ ground{regRead = regread,
                                   writeTemp = True} 

condJump :: Label -> Instruction
condJump label = IntI $  D {isCond = False,
                            addr = label
                           } 

fetchCar :: Reg -> Reg -> Instruction
fetchCar regread regwrite = ExtI $ ground{regRead = regread,
                                      regWrite = regwrite
                                     } 
 
fetchCdr :: Reg -> Reg -> Instruction
fetchCdr regread regwrite = ExtI $ ground{regRead = regread,
                                      regWrite = regwrite,
                                      gcOpcode = (False,True)
                                     } 

fetchCarTemp :: Reg -> Instruction  
fetchCarTemp reg1 = ExtI $ ground{ regRead = reg1 ,
                               writeTemp = True}


fetchCdrTemp :: Reg -> Instruction
fetchCdrTemp regread = ExtI $ ground{regRead = regread,
                                 writeTemp = True,
                                 gcOpcode = (False,True)
                                }

allocCons :: Reg -> Reg -> Instruction --By default tag = List
allocCons input output = ExtI $ ground{regRead = input, 
                                   writeFlag = True,
                                   writeTemp = True,
                                   regWrite = output,
                                   gcOpcode = (True,True),
                                   immediate = [True,False,True,False,False] 
                                               ++ replicate 4 False
                                  }

allocConsWithTag :: Reg -> Reg -> [Bool] -> Instruction
allocConsWithTag input output tag = ExtI $ ground{regRead = input, 
                                   writeFlag = True,
                                   writeTemp = True,
                                   regWrite = output,
                                   gcOpcode = (True,True),
                                   immediate = tag
                                  }

----- Administrative tools dedicate to the assembling.-----

regToString Value = "000"  
regToString Expr  = "001"
regToString Env   = "010"
regToString Args  = "011"
regToString Stack = "100"
regToString Temp  = "101"

ground  = C {regRead = Value,
           regWrite = Value,
           writeFlag = False,
           writeTemp = False,
           muxData = False,
           gcOpcode = (False,False),
           aluCtrl = False,
           useAlu = False,
           immediate = replicate 9 False,
           loadCondReg = False}



printBool :: Bool -> String
printBool True  = "1"  
printBool False = "0"


printAddr :: Int -> Int -> String
printAddr microAddr pos = 
  take microAddr $ aux pos ++ repeat '0' -- Lazyness rocks!!! 
 where aux pos = 
        if pos == 0 then [] else 
          (show $ pos `mod` 2) ++ aux (pos `div` 2)

           


 
