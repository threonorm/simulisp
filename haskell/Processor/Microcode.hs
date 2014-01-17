module Processor.Microcode where

import Control.Arrow

import Assembler.Assembler
import Processor.Parameters
import Data.List

blockSize = microAddrS - tagS

selfEvaluating = [ Value ^= Expr,
                   Dispatch Stack ]

label x = Label (x, Nothing)


eval = [ (TNil     , selfEvaluating)
       , (TClosure , selfEvaluating)
       , (TList    , selfEvaluating)
       , (TNum     , selfEvaluating)
         
       , (TLocal   , lookupLocal ++
                     [Dispatch Stack])
       , (TGlobal  , [ fetchCar Expr Value
                     , Dispatch Stack ])
         
       , (TCond    , [ loadConditional Value
                     , condJump "condNotNil"
                     , fetchCdr Expr Expr
                     , label "condNotNil"
                     , fetchCar Expr Expr ])
         
       , (TProc    , [ moveToTemp Env
                     , allocConsWithTag Expr Value (tagBin TClosure)
                     , Dispatch Stack])

       , (TFirst   , saveCdrAndEvalCar RFirst)
       , (TNext    , push Args ++
                     saveCdrAndEvalCar RNext)
       , (TLast    , push Args ++
                     saveCdrAndEvalCar RLast)
         
       , (TApplyOne, saveCdrAndEvalCar RApplyOne)
       , (TLet     , saveCdrAndEvalCar RLet)
       , (TSequence, saveCdrAndEvalCar RSequence)
       ]
       ++
       [ (prim     , selfEvaluating)
       | prim <- [TCar, TCdr, TCons, TIncr, TDecr, TIsZero,
                  TIsgt60, TIsgt24, TPrintSec, TPrintMin, TPrintHour] ]

-- kind of a todo list...
registerNull = undefined
allocSingleton src dest = [ moveToTemp src
                          , allocCons registerNull dest ]
lookupLocal = undefined
doEval = Dispatch Expr
doApply = undefined
getLastArg = fetchCar Args Value
fetchCarAndIncr = undefined
fetchCarAndDecr = undefined
fetchCarAndDecrRetrievingComp = undefined
fetchCarAndSubImmediate = undefined
pushWithReturn = undefined

apply = [ (t, allocSingleton Args Env ++
              [ Expr ^= Value
              , doEval ])
        | t <- [TNil, TList, TNum, TLocal, TGlobal, TCond, TProc,
                TFirst, TNext, TLast, TApplyOne, TLet, TSequence]
        ]
        ++
        map (second (++ [Dispatch Stack]))
        [ (TCar      , [ getLastArg
                       , fetchCar Value Value ])
        , (TCdr      , [ getLastArg
                       , fetchCdr Value Value ])
        , (TCons     , [ getLastArg
                       , moveToTemp Value
                       , fetchCdr Args Value
                       , fetchCar Value Value
                       , allocCons Value Value ])
        , (TIncr     , [ fetchCarAndIncr Args Value ])
        , (TDecr     , [ fetchCarAndDecr Args Value ])
        , (TIsZero   , [ fetchCarAndDecrRetrievingComp Args Value ])
        , (TIsgt60   , [ fetchCarAndSubImmediate Args Value 60 ])
        , (TIsgt24   , [ fetchCarAndSubImmediate Args Value 24 ])
        , (TPrintSec , undefined)
        , (TPrintMin , undefined)
        , (TPrintHour, undefined)
        ]

popToReg reg = [ fetchCar Stack reg
               , fetchCdr Stack Stack ]
standardRestore = popToReg Expr ++
                  popToReg Env

return = [ (RFirst    , standardRestore ++
                        allocSingleton Value Args ++
                        [ doEval ])
           
         , (RNext     , standardRestore ++
                        popToReg Args ++
                        [ moveToTemp Value
                        , allocCons Args Args
                        , doEval ])
           
         , (RLast     , standardRestore ++
                        popToReg Args ++
                        [ moveToTemp Value
                        , allocCons Args Args ] ++
                        pushWithReturn Args RApply ++
                        [ doEval ])
           
         , (RApplyOne , standardRestore ++
                        allocSingleton Value Args ++
                        pushWithReturn Args RApply ++
                        [ doEval ])
           
         , (RLet      , standardRestore ++
                        allocSingleton Value Value ++
                        [ moveToTemp Value
                        , allocCons Env Env
                        , doEval ])

         , (RSequence , standardRestore ++
                        [ doEval ])

         , (RApply    , popToReg Args ++
                        [ doApply ])
         ]
                    

push :: Reg -> [Instruction]                 

push reg = [moveToTemp reg,allocCons Stack Stack]

-- pushWithReturn :: [Bool] -> Reg -> [Instruction]
-- pushWithReturn retTag reg = [moveToTemp reg,allocConsWithTag Stack Stack retTag]

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

                                
