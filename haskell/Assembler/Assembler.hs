{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}


module Assembler  where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix

data Reg = Value
    | Expr
    | Env
    | Args
    | Stack          
    | Temp

regToString Value = "000"  
regToString Expr  = "001"
regToString Env   = "010"
regToString Args  = "011"
regToString Stack = "100"
regToString Temp  = "101"

data Instruction =
    | ExtI ExternalInstruction
    | IntI InternalInstruction
    | Label String 

data ExternalInstruction = 
    { regRead Reg,
      regWrite Reg,
      writeFlag Bool,
      writeTemp Bool,
      muxData Bool,
      gcOpcode (Bool,Bool),
      aluCtrl Bool,
      useAlu Bool}

data InternalInstruction =
    {isCond :: Bool,
     addr :: Label
    }

type Label = String 

type code = [Instruction]

ground  = {regRead = Value,
           regWrite = Value,
           writeFlag = False,
           writeTemp = False,
           muxData = False,
           gcOpcode = (False,False),
           aluCtrl = False,
           useAlu = False}


(:=) :: Reg -> Reg -> Instruction
reg1 := reg2 =  ExternalInstruction ground{regRead =  reg2,
                       regWrite = reg1,
                       writeFlag = True 
                       }

jmp :: Label ->  Instruction 
jmp label =
InternalInstruction {isCond = false,
                    addr = label  
                    }

condJump :: Label -> Instruction
condJump label = 
InternalInstruction {isCond = false,
                     addr = label
                    } 

fetchCar :: Reg -> Reg -> Instruction
fetchCar regread regwrite =
ExternalInstruction {regRead = regread,
                     regWrite = regwrite,
                    } 
 
fetchCdr :: Reg -> Reg -> Instruction
fetchCdr regread regwrite =
ExternalInstruction {regRead = regread,
                     regWrite = regwrite,
                     gcOpcode = (false,true)
                    } 

fetchCarTemp :: Reg -> Instruction  
fetchCarTemp reg1 =
ExternalInstruction{ regRead = reg1 ,
                     writeTemp = True}


fetchCdrTemp :: Reg -> Instruction
fetchCdrTemp regread =
ExternalInstruction {regRead = regread,
                     writeTemp = True,
                     gcOpcode = (False,True)
                    }

allocCons :: Reg -> Reg -> Instruction
allocCons input output = 
ExternalInstruction {regRead = input, 
                     writeFlag = True,
                     writeTemp = True,
                     regWrite = output,
                     gcOpcode = (True,True)
                    }

type Intermediate =  Either String Instruction 
                    deriving (Show,Eq) 

printBool :: Bool -> String
printBool True  = "1"  
printBool False = "0"



assembleFirst :: Instruction -> Intermediate
assembleFirst (ExternalInstruction instr) =
Left ("0"++                                      -- Bit of internal incr 
 (regToString . regRead $  inst ) ++
 (regToString . regWrite $  inst ) ++
 (printBool . writeFlag $  inst ) ++
 (printBool . writeTemp $  inst ) ++
 (printBool . muxDatap $  inst ) ++
 (printBool . fst . gcOpcode $  inst ) ++
 (printBool . snd . gcOpcode $ inst ) ++
 (printBool . aluCtrl $ inst ) ++
 (printBool . useAlu $ inst ))
 
assembleFirst (InternalInstruction instr) = 
  Right (InternalInstruction instr) 


assembleFirst (Label blabla) =
  Right (Label blabla)

assembleSecond :: [Intermediate] -> String
assembleSecond [] = []  
assembleSecond ((Left blabla):q) = ("0"++blabla):(assembleSecond q)
assembleSecond code@((Right(InternalInstruction instr)): q) =  
 ("1"++ -- Bit of Jump
 (
  (printBool . isCond $ instr ++
   printAddr 12  . position . addr $ instr--12 is the microAddrS of Processor  
   )++
 "0"  --Useless bit in this case
 )) : assembleSecond q
    
assembleSecond ((Right(Label string)): q) = 
  assembleSecond q
 

position :: Label -> [Intermediate] -> Int
position lab ((Right (Label s))::q) =
 if lab=s then 1 else 1 + position lab q

  
printAddr :: Int -> Int -> String
printAddr microAddr pos = 
  take microAddr $ aux pos ++ repeat 0 -- Lazyness rocks!!! 
 where aux pos = 
        if pos = 0 then [] else 
          (show $ pos `mod` 2) ++ aux (pos/2)

 
