module Simulator where

import Control.Applicative
import Data.List
import qualified Data.Map as Map

import NetlistAST
import Scheduler
import qualified Digraph as G


newtype SysState = 
  SysState{val_vars :: Map.Map ident Value}

extractArg :: SysState -> Arg -> Value
extractArg st (Avar i) = st Map.! i 
extractArg st (Aconst v) = v


vLogic:: Binop -> Value -> Value -> Value
vLogic op v1 v2 =
  case (v1,v2) of
    (VBit a1,VBit a2)->VBit
    (VBitArray l1, VBitArray l2)-> VBitArray
    (VBitArray l1,VBit a1) -> VBitArray
    (VBit a1,VBitArray l1) -> VBitArray

vNot :: Value -> Value
vNot v= case v of 
 (VBit a) -> VBit (not a)
 (VBitArray l) -> VBitArray (map not $ l) 


simulate :: SysState -> Exp -> Value




stepSimulation :: Program -> SysState -> SysState
  
