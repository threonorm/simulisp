module Simulator where

import Control.Applicative
import Data.List
import qualified Data.Map as Map

import NetlistAST
import Scheduler
import qualified Digraph as G


newtype SysState = 
  SysState{val_vars :: Map.Map Ident Value}

extractArg :: SysState -> Arg -> Value
extractArg st (Avar i) = val_vars st Map.! i 
extractArg st (Aconst v) = v


vLogic:: (Bool->Bool->Bool) -> Value -> Value -> Value
vLogic op v1 v2 =
  case (v1,v2) of
    (VBit a1,VBit a2)->VBit (op a1 a2)
    (VBitArray l1, VBitArray l2)-> VBitArray (map (\(a,b)->op a b) $ zip l1 l2)
    (VBitArray l1,VBit a1) ->
         VBitArray (map (\(a,b)->op a b) $ zip l1 (repeat a1))
    (VBit a1,VBitArray l1) -> 
         VBitArray (map (\(a,b)->op a b) $ zip l1 (repeat a1))

vNot :: Value -> Value
vNot v= case v of 
 (VBit a) -> VBit (not a)
 (VBitArray l) -> VBitArray (map not $ l) 


--simulate :: SysState -> Exp -> Value


--stepSimulation :: Program -> SysState -> SysState
  
