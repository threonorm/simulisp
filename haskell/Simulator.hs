module Simulator where

import Control.Applicative
import Data.List
import qualified Data.Map as Map
import Data.Bool

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
    (VBitArray l1,VBit a1) ->  --Intuition : maybe it is not necessary
         VBitArray (map (\(a,b)->op a b) $ zip l1 (repeat a1))
    (VBit a1,VBitArray l1) ->  --Idem : I'm lazy and don't want read the specs
         VBitArray (map (\(a,b)->op a b) $ zip l1 (repeat a1))


vNot :: Value -> Value
vNot v= case v of 
 (VBit a) -> VBit (not a)
 (VBitArray l) -> VBitArray (map not $ l) 


u :: Value -> [Bool]
u (VBitArray x) = x   -- u for under : through the constructor


simulate :: SysState -> Exp -> Value
simulate st (Earg a) =
  extractArg st a
simulate st (Enot a) =
  vNot . extractArg st $ a
simulate st (Ebinop op a1 a2) =
  vLogic opTransf vA1 vA2
  where vA1 = extractArg st a1
        vA2 = extractArg st a2 
        opTransf = transfo op
        transfo x = case x of
           And  -> (&&) 
           Nand -> (\p q-> not $ p && q)
           Xor  ->(\p q->(p || q) && not (p && q)) 
           Or   -> (||)
simulate st (Emux a1 a2 a3) =
  case vA1 of
    VBitArray _ ->
           VBitArray (zipWith3 (\x y z ->if x then y else z) 
                      (u vA1) 
                      (u vA2) 
                      (u vA3))
    VBit v1 ->
           if v1 then vA2 else vA3
  where vA1 = extractArg st a1
        vA2 = extractArg st a2
        vA3 = extractArg st a3
simulate st (Eselect i a)=
  VBit ((u $ extractArg st a  ) !! i)
simulate st (Eslice i1 i2 a)=
  VBitArray ( take (i2 - i1) . drop i1 . u $ extractArg st a )
simulate st (Econcat a1 a2) =
  VBitArray ( (u vA1) ++ (u vA2) )   
  where vA1 = extractArg st a1
        vA2 = extractArg st a2 

stepSimulation :: SysState -> Equation -> SysState
stepSimulation st eq =
  SysState{val_vars = 
        Map.insert (fst eq) (simulate st (snd eq)) $ val_vars st}        

--We could delete the overhead SysState, juste for a Map.Map

simulation :: Program -> SysState -> SysState   
simulation prog input =
 foldl' stepSimulation input $ p_eqs prog 
