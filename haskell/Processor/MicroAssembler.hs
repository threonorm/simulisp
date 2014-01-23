module Processor.MicroAssembler where

import Control.Arrow
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Processor.Parameters
import Lisp.SCode
import Lisp.Primitives
import Util.BinUtils

-- There are three types of instructions : Labels, classical instructions using
-- the mini ALU and finishing by the incrementation of the micro program counter,
-- and jumps (conditional or not).

data Instruction = ActualInstr MicroInstruction
                 | JumpLabel Bool String
                 | Label String (Maybe Int) -- "Just n": align on address n

-- 2 passes of assembling which absolutely do not concern themselves
-- with turning the microprogram into a sequence of zeroes and ones
-- only aligning the addresses and resolving labels and jumps correctly

-- First pass: resolve and eliminate labels, and give explicit positions

firstPass :: [Instruction] -> (Map String Int, [(Int, Instruction)])
firstPass = go 0
  where go _ [] = (Map.empty, [])
        go i ((Label l (Just j)) : xs)
          | i <= j = let (labels, instructions) = go j xs in
                     (Map.insert l j labels, instructions)
          | otherwise = error "alignment issues"
        go i ((Label l Nothing) : xs) =
          let (labels, instructions) = go i xs in
          (Map.insert l i labels, instructions)
        go i (x:xs) =
          let (labels, instructions) = go (i+1) xs in
          (labels, (i, x) : instructions)

-- Second pass: eliminate named jumps

secondPass :: (Map String Int, [(Int, Instruction)]) -> [(Int, MicroInstruction)]
secondPass (labels, instructions) = map (second f) instructions
  where f (ActualInstr mi) = mi
        f (JumpLabel cond l) = Jump { jumpIsConditional = cond
                                    , jumpAddress = labels Map.! l }
        f _ = error "You should eliminate labels first with firstPass."


-- Third pass: actually generate a binary string

assembleMicrocode :: [Instruction] -> String
assembleMicrocode = g . f 0 . secondPass . firstPass
  where f _ [] = []
        f currentAddr instructions@((wantedAddr, instruction):rest)
          | wantedAddr < currentAddr = error "assembly doesn't fit????"
          | wantedAddr > currentAddr = padding microInstrS
                                       ++ f (currentAddr+1) instructions
          | otherwise = assembleInstruction instruction
                        ++ f (currentAddr+1) rest
        g = map (\b -> if b then '1' else '0')


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


doGC :: GCOp -> Immediate -> Reg -> Reg -> Instruction
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
fetchCar = doGC GCFetchCar (ImmN 0)
 
fetchCdr :: Reg -> Reg -> Instruction
fetchCdr = doGC GCFetchCdr (ImmN 0)

allocCons :: Reg -> Reg -> Instruction -- By default tag = List
allocCons = allocConsWithTag TList

allocConsWithTag :: Tag -> Reg -> Reg -> Instruction
allocConsWithTag tag = doGC GCAlloc (ImmT tag)

allocConsWithReturn :: ReturnTag -> Reg -> Reg -> Instruction
allocConsWithReturn ret = doGC GCAlloc (ImmR ret)

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
printSec, printMin, printHour, printDay, printMonth, printYear :: Reg -> Instruction
[printSec, printMin, printHour, printDay, printMonth, printYear] =
  [  printSth (decToBools 3 x) | x <- [1..6] ]

sync :: Instruction
sync = makeInstr $ ground { interactWithOutside = True
                          , outsideOpcode = [True, True, True] }

----- Administrative tools dedicate to the assembling.-----

ground :: ExternalInstruction
ground = CS { regRead = Null
            , regWrite = Null          
            , writeReg = False   
            , writeTemp = False        
            , useGC = False       
            , gcOpcode = GCNop
            , aluCtrl = ALUNop
            , loadCondReg = False        
            , interactWithOutside = False
            , outsideOpcode = [False, False, False]
            , immediate = ImmN 0
            }

 
