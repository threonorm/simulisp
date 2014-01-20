module Processor.MicroAssembler where

import Data.List
import Processor.Parameters

--------- MiniAssembler V0.1 -------------


data Instruction = ActualInstr MicroInstruction
                 | JumpLabel Bool String
                 | Label String (Maybe Int) -- "Just n": align on address n

-- --There are three types of instructions : Labels, classical instructions using
-- --the mini ALU and finishing by the incrementation of the micro program counter,
-- --and jumps (conditional or not).


-- data InternalInstruction =
--     D {isCond :: Bool,
--      addr :: Label
--     }

-- type Label =  String


-- data Reg = Null
--          | Value
--          | Expr
--          | Env
--          | Args
--          | Stack
--          | Temp

-- type Intermediate =  Either String Instruction 

--MiniAssembler makes two passes

-- assemble :: [Instruction] -> Maybe String 
-- assemble code =
--   if checkPosition inter 0 
--   then Just $ assembleSecond inter 0 inter 
--   else Nothing
--   where inter =  map assembleFirst code


-- assembleFirst :: Instruction -> Intermediate
-- assembleFirst (ActualInstr instr) = Left ("0"++                -- Bit of internal incr 
--  (regToString . regRead $  instr ) ++
--  (regToString . regWrite $  instr ) ++
--  (printBool . writeFlag $  instr ) ++
--  (printBool . writeTemp $  instr ) ++
--  (printBool . muxData $  instr ) ++
--  (printBool . fst . gcOpcode $  instr ) ++
--  (printBool . snd . gcOpcode $ instr ) ++
--  (printBool . aluCtrl $ instr ) ++
--  (printBool . useAlu $ instr )++
--  replicate (microInstrS-15) '0')  --Padding immediate + lgr

-- -- assembleFirst (Dispatch reg)=
-- --   Left ( "1" ++
-- --          regToString reg ++
-- --          replicate 19 '0') -- Padding with 0 
-- -- assembleFirst (IntI instr) = 
-- --   Right (IntI instr) 
-- -- assembleFirst (Label blabla) =
-- --   Right (Label blabla)



-- assembleSecond :: [Intermediate] -> Int -> [Intermediate] -> String  -- Code,position, list from position, label
-- assembleSecond code pos [] = []
-- assembleSecond code pos ((Left blabla):q) = "0"++blabla++assembleSecond code (pos+1) q

-- assembleSecond code pos ((Right(IntI instr)): q) =  
--  ("1"++ -- Bit of Jump
--   (printBool . isCond $ instr) ++
--   ( printAddr 12 . position code pos. addr $ instr) ++
--     replicate (microInstrS - microAddrS - 2) '0'  --Useless bit in this case
--  ) ++ assembleSecond code (pos+1) q
-- assembleSecond code pos ((Right(Label s (Just t))): q) = 
--   (take (microInstrS *floodSize) $ repeat '0') ++ assembleSecond code t q
--   where floodSize = t-pos 
-- assembleSecond code pos ((Right(Label s Nothing)): q) = 
--    assembleSecond code (pos) q


-- --Give the position of the declaration of the Label in the Code (in words).
-- position :: [Intermediate] -> Int ->  Label -> Int
-- position ((Right (Label s (Just t))):q) pos lab =
--  if s==lab then t else position q pos lab

-- position ((Right (Label s Nothing)):q) pos lab =
--  if s==lab then pos else position q (pos) lab
-- position (_:q) pos lab =  position q (pos+1) lab

-- --To check if the assembly is possible with the alignment required by the user.
-- checkPosition :: [Intermediate] -> Int -> Bool 
-- checkPosition [] prec = True  
-- checkPosition (Right(Label s (Just t)):q) prec = if prec <= t then checkPosition q t 
--              else False
-- checkPosition (Right(Label s Nothing):q) prec = checkPosition q prec 
-- checkPosition (_:q) prec = checkPosition q $ prec + 1  --In number of words

---------- MiniASM as a sort of an eDSL  ---------

jmp :: String -> Instruction
jmp = JumpLabel False

condJump :: String -> Instruction
condJump = JumpLabel True

makeInstr :: ExternalInstruction -> Instruction
makeInstr = ActualInstr . ExtInstr
           
(^=) :: Reg -> Reg -> Instruction
dest ^= src = makeInstr $ ground { regRead = src
                                 , regWrite = dest
                                 , writeReg = True 
                                 }

moveToTemp :: Reg -> Instruction
moveToTemp src = makeInstr $ ground { regRead = src
                                    , writeTemp = True
                                    } 

loadConditional :: Reg -> Instruction
loadConditional reg = makeInstr $ ground { regRead = reg
                                         , loadCondReg = True 
                                         }


doGC :: [Bool] -> Immediate -> Reg -> Reg -> Instruction
doGC opc imm src Temp = makeInstr $ ground { regRead = src
                                           , writeTemp = True
                                           , gcOpcode = opc
                                           , useGC = True
                                           , immediate = imm
                                           } 
doGC opc imm src dest = makeInstr $ ground { regRead = src
                                           , writeReg = True
                                           , regWrite = dest
                                           , gcOpcode = opc
                                           , useGC = True
                                           , immediate = imm
                                           } 

fetchCar :: Reg -> Reg -> Instruction
fetchCar = doGC [False, False] (ImmN 0)
 
fetchCdr :: Reg -> Reg -> Instruction
fetchCdr = doGC [False, True] (ImmN 0)

allocCons :: Reg -> Reg -> Instruction -- By default tag = List
allocCons = allocConsWithTag TList

allocConsWithTag :: Tag -> Reg -> Reg -> Instruction
allocConsWithTag tag = doGC [True, False] (ImmT tag)

allocConsWithReturn :: ReturnTag -> Reg -> Reg -> Instruction
allocConsWithReturn ret = doGC [True, False] (ImmR ret)

doALU :: ALUOp -> Int -> Reg -> Reg -> Instruction
-- dest=Null => no write
doALU op imm src Null = makeInstr $ ground { regRead = src
                                           , useGC = False
                                           , aluCtrl = op
                                           , immediate = ImmN imm
                                           }
doALU op imm src Temp = makeInstr $ ground { regRead = src
                                           , writeTemp = True
                                           , useGC = False
                                           , aluCtrl = op
                                           , immediate = ImmN imm
                                           }
doALU op imm src dest = makeInstr $ ground { regRead = src
                                           , writeReg = True
                                           , regWrite = dest
                                           , useGC = False
                                           , aluCtrl = op
                                           , immediate = ImmN imm
                                           }

printSth :: [Bool] -> Reg -> Instruction
printSth op src = makeInstr $ ground { regRead = src
                                     , interactWithOutside = True
                                     , outsideOpcode = op }
printSec, printMin, printHour :: Reg -> Instruction
printSec  = printSth [False, True]
printMin  = printSth [True, False]
printHour = printSth [False, False]

sync :: Instruction
sync = makeInstr $ ground { interactWithOutside = True
                          , outsideOpcode = [True, True] }

----- Administrative tools dedicate to the assembling.-----

regToString Value = "000"  
regToString Expr  = "001"
regToString Env   = "010"
regToString Args  = "011"
regToString Stack = "100"
regToString Temp  = "101"


ground :: ExternalInstruction
ground = CS { regRead = Null
            , regWrite = Null          
            , writeReg = False   
            , writeTemp = False        
            , useGC = False       
            , gcOpcode = [False, False]
            , aluCtrl = ALUNop
            , loadCondReg = False        
            , interactWithOutside = False
            , outsideOpcode = [False, False]
            , immediate = ImmN 0
            }



  



printBool :: Bool -> String
printBool True  = "1"  
printBool False = "0"


printAddr :: Int -> Int -> String
printAddr microAddr pos = 
  take microAddr $ aux pos ++ repeat '0' -- Lazyness rocks!!! 
 where aux pos = 
        if pos == 0 then [] else 
          (show $ pos `mod` 2) ++ aux (pos `div` 2)

           


 
