module Processor.Microcode where

import Assembler.Assembler
import Processor.Parameters
import Data.List


selfEvaluating = [ Value ^= Expr,
                   Dispatch Stack ]

blockSize = microAddrS - tagS

processor =  (Label ("Nil",0):selfEvaluating) ++ 

             [Label ("Local",blockSize)] ++
                  walkOnList ++
                  [Dispatch Stack] ++

             [Label ("Global",2*blockSize),
              fetchCar Expr Value,
              Dispatch Stack]++

             (Label ("Closure",3*blockSize): selfEvaluating)++

             [Label("Cond",4*blockSize) ] ++ -- TODO cond

             (Label ("List",5*blockSize): selfEvaluating)++
             
             (Label ("Num",6*blockSize): selfEvaluating)++
             
             [Label("Proc",7*blockSize),
              moveToTemp Env,
              allocConsWithTag Expr Value [True,True,False,False,False],
             Dispatch Stack
             ]++  
             
              [Label("First",8*blockSize)]++
              saveCdrAndEvalCar returnFirst ++ 
              
              [Label("Next",9*blockSize),
               moveToTemp Args,
               allocCons Stack Stack]++
               saveCdrAndEvalCar returnNext ++
              
              [Label("Last",10*blockSize),
               moveToTemp Args,
               allocCons Stack Stack]++
               saveCdrAndEvalCar returnLast ++
              
              (Label("ApplyOne",11*blockSize):
               saveCdrAndEvalCar returnApplyOne) ++

              (Label("Let",12*blockSize):
               saveCdrAndEvalCar returnLet) ++              
               
              --TODO pritimive
              (Label("Sequence", undefined):
               saveCdrAndEvalCar returnSequence) 


              

push :: Reg -> [Instruction]                 

push reg = [moveToTemp reg,allocCons Stack Stack]

pushWithReturn :: [Bool] -> Reg -> [Instruction]
pushWithReturn retTag reg = [moveToTemp reg,allocConsWithTag Stack Stack retTag]

walkOnList = undefined

returnFirst =  undefined
returnNext = undefined 
returnLast = undefined
returnLet      = undefined    -- Perfect indentation: Vim power  
returnSequence = undefined  
returnApplyOne = undefined 

saveCdrAndEvalCar returnAddr = push Env ++
                                [fetchCdrTemp Expr]++
                                pushWithReturn returnAddr Expr++
                                [fetchCar Expr Expr,
                                Dispatch Expr] 

                                
