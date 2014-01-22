module Processor.Microcode (microprogram) where

import Prelude hiding (return)

import Control.Arrow
import Data.Function
import Data.List

import Processor.MicroAssembler
import Processor.Parameters
import Lisp.SCode
import Lisp.Primitives

selfEvaluating = [ Value ^= Expr,
                   dispatchReturn ]

label x = Label x Nothing


-- program start at address 0
boot = [ fetchCar Null Expr
       , dispatchEval ]

eval = [ (TNil     , selfEvaluating)
       , (TClosure , selfEvaluating)
       , (TList    , selfEvaluating)
       , (TNum     , selfEvaluating)
         
       , (TLocal   , lookupLocal ++
                     [ dispatchReturn ])
       , (TGlobal  , [ fetchCar Expr Value
                     , dispatchReturn ])
         
       , (TCond    , [ loadConditional Value
                     , condJump "condNotNil"
                     , fetchCdr Expr Expr
                     , dispatchEval
                     , label "condNotNil"
                     , fetchCar Expr Expr
                     , dispatchEval ])
         
       , (TProc    , [ moveToTemp Env
                     , allocConsWithTag TClosure Expr Value
                     , dispatchReturn ])

       , (TFirst   , saveCdrAndEvalCar RFirst)
       , (TNext    , push Args ++
                     saveCdrAndEvalCar RNext)
       , (TLast    , push Args ++
                     saveCdrAndEvalCar RLast)
         
       , (TApplyOne, saveCdrAndEvalCar RApplyOne)
       , (TLet     , saveCdrAndEvalCar RLet)
       , (TSequence, saveCdrAndEvalCar RSequence)
         
       , (TSync    , [ fetchCar Expr Expr
                     , sync
                     , dispatchEval ])
       ]
       ++
       [ (TPrim prim, selfEvaluating)
       | prim <- [PCar, PCdr, PCons, PIncr, PDecr, PIsZero,
                  PIsgt60, PIsgt24, PPrintSec, PPrintMin, PPrintHour] ]


-- note: decrupper must leave its argument zero if it was zero

lookupLocal = [ label "local_outerloop"
              -- sets condition register to (expr.upper = 0)?
              , doALU ALUDecrUpper 0 Expr Expr
              , condJump "local_endouterloop"
              , fetchCdr Env Env
              , jmp "local_outerloop"
              , label "local_endouterloop"
              , fetchCar Env Env
              , label "local_innerloop"
              , doALU ALUDecrImmediate 1 Expr Expr
              , condJump "local_endinnerloop"
              , fetchCdr Env Env
              , jmp "local_innerloop"
              , label "local_endinnerloop"
              , fetchCar Env Value
              ]


-- kind of a todo list...
allocSingleton src dest = [ moveToTemp src
                          , allocCons Null dest ]
getLastArg = fetchCar Args Value

dispatchEval   = ActualInstr $ Dispatch Expr  evalSuffix
dispatchApply  = ActualInstr $ Dispatch Expr  applySuffix
dispatchReturn = ActualInstr $ Dispatch Stack returnSuffix

apply = [ (t, allocSingleton Args Env ++
              [ Expr ^= Value
              , dispatchEval ])
        | t <- [TNil, TList, TNum, TLocal, TGlobal, TCond, TProc,
                TFirst, TNext, TLast, TApplyOne, TLet, TSequence, TSync]
        ]
        ++
        map (TPrim *** (++ [dispatchReturn]))
        [ (PCar      , [ getLastArg
                       , fetchCar Value Value ])
        , (PCdr      , [ getLastArg
                       , fetchCdr Value Value ])
        , (PCons     , [ getLastArg
                       , moveToTemp Value
                       , fetchCdr Args Value
                       , fetchCar Value Value
                       , allocCons Value Value ])
        , (PIncr     , [ fetchCar Args Value
                       , doALU ALUIncr 0 Value Value ])
        , (PDecr     , [ fetchCar Args Value
                       , doALU ALUDecrImmediate 1 Value Value])
        , (PIsZero   , [ fetchCar Args Value
                       , doALU ALUDecrImmediate 1 Value Null
                       , condJump "zero"
                       , Value ^= Null
                       , dispatchReturn
                       , label "zero"
                       , doALU ALUIncr 0 Null Value -- using Num(1) as constant True
                       , dispatchReturn
                       ])
        , (PIsgt60   , [ fetchCar Args Value
                       , doALU ALUDecrImmediate 60 Value Null
                       , condJump "strictlt60"
                       , doALU ALUIncr 0 Null Value
                       , dispatchReturn
                       , label "strictlt60"
                       , Value ^= Null
                       , dispatchReturn])
        , (PIsgt24   , [ fetchCar Args Value
                       , doALU ALUDecrImmediate 24 Value Null
                       , condJump "strictlt24"
                       , doALU ALUIncr 0 Null Value
                       , dispatchReturn
                       , label "strictlt24"
                       , Value ^= Null
                       , dispatchReturn ])
        , (PPrintSec , [ getLastArg
                       , printSec Value ])
        , (PPrintMin , [ getLastArg
                       , printMin Value ])
        , (PPrintHour , [ getLastArg
                        , printHour Value ])
        ]

popToReg reg = [ fetchCar Stack reg
               , fetchCdr Stack Stack ]
standardRestore = popToReg Expr ++
                  popToReg Env

return = [ (RFirst    , standardRestore ++
                        allocSingleton Value Args ++
                        [ dispatchEval ])
           
         , (RNext     , standardRestore ++
                        popToReg Args ++
                        [ moveToTemp Value
                        , allocCons Args Args
                        , dispatchEval ])
           
         , (RLast     , standardRestore ++
                        popToReg Args ++
                        [ moveToTemp Value
                        , allocCons Args Args 
                        , moveToTemp Args
                        , allocConsWithReturn RApply Stack Stack
                        , dispatchEval ])
           
         , (RApplyOne , standardRestore ++
                        allocSingleton Value Args ++
                        pushWithReturn RApply Args ++
                        [ moveToTemp Args
                        , allocConsWithReturn RApply Stack Stack
                        , dispatchEval ])
           
         , (RLet      , standardRestore ++
                        allocSingleton Value Value ++
                        [ moveToTemp Value
                        , allocCons Env Env
                        , dispatchEval ])

         , (RSequence , standardRestore ++
                        [ dispatchEval ])

         , (RApply    , popToReg Args ++
                        [ dispatchApply ])
         ]

microprogram :: [Instruction]
microprogram = boot ++ eval' ++ apply' ++ return'
  where eval'   = concat [ Label ("E" ++ show tag) (Just $ evalAddr tag) : code
                         | (tag, code) <- sortBy (compare `on` (evalAddr . fst)) eval ]
        apply'  = concat [ Label ("A" ++ show tag) (Just $ applyAddr tag) : code
                         | (tag, code) <- sortBy (compare `on` (applyAddr . fst)) apply ]
        return' = concat [ Label (show tag) (Just $ returnAddr tag) : code
                         | (tag, code) <- sortBy (compare `on` (returnAddr . fst)) return ]
        evalAddr   = addressOfTag evalSuffix   . tagNum
        applyAddr  = addressOfTag applySuffix  . tagNum
        returnAddr = addressOfTag returnSuffix . returnNum
        smallBlockSize = 2^(microAddrS - tagS - 2)
        bigBlockSize   = 2^(microAddrS - 2)
        addressOfTag [b0,b1] i = j*bigBlockSize + i*smallBlockSize
          where j = 2*b1' + b0'
                b1' | b1 = 1
                    | otherwise = 0
                b0' | b0 = 1
                    | otherwise = 0
        

push :: Reg -> [Instruction]                 
push reg = [ moveToTemp reg
           , allocCons Stack Stack]

pushWithReturn :: ReturnTag -> Reg -> [Instruction]
pushWithReturn retTag reg = [ moveToTemp reg
                            , allocConsWithReturn retTag Stack Stack]

saveCdrAndEvalCar :: ReturnTag -> [Instruction]
saveCdrAndEvalCar returnTag = push Env ++
                              [ fetchCdr Expr Temp
                              , allocConsWithReturn returnTag Stack Stack
                              , fetchCar Expr Expr
                              , dispatchEval ] 
                                
