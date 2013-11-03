module Simulator where

import Control.Applicative
import Control.Monad
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


compute :: SysState -> Exp -> Value
compute st (Earg a) =
  extractArg st a
compute st (Enot a) =
  vNot . extractArg st $ a
compute st (Ebinop op a1 a2) =
  vLogic opTransf vA1 vA2
  where vA1 = extractArg st a1
        vA2 = extractArg st a2 
        opTransf = transfo op
        transfo x = case x of
           And  -> (&&) 
           Nand -> (\p q-> not $ p && q)
           Xor  ->(\p q->(p || q) && not (p && q)) 
           Or   -> (||)
compute st (Emux a1 a2 a3) =
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
compute st (Eselect i a)=
  VBit ((u $ extractArg st a  ) !! i)
compute st (Eslice i1 i2 a)=
  VBitArray ( take (i2 - i1) . drop i1 . u $ extractArg st a )
compute st (Econcat a1 a2) =
  VBitArray ( (u vA1) ++ (u vA2) )   
  where vA1 = extractArg st a1
        vA2 = extractArg st a2 

simulationStep :: SysState -> Equation -> SysState
simulationStep st eq =
  SysState{val_vars = 
        Map.insert (fst eq) (compute st (snd eq)) $ val_vars st}        

--We could delete the overhead SysState, juste for a Map.Map

simulateCycle :: Program -> SysState -> SysState   
simulateCycle prog input =
  foldl' simulationStep input $ p_eqs prog 

-- testing function
-- TODO: factor out the scaffolding to reuse it more generally
simulateOneCycle :: Program -- Sorted netlist
                 -> Maybe (Environment Value) -- Map of inputs
                 -> Maybe [(Ident, Value)] -- outputs with possibility of error
                                           -- TODO: refine error signaling
simulateOneCycle prog maybeInputs =
  gatherOutputs . simulateCycle prog . SysState <$> initialState
  where formalParams = p_inputs prog
        initialState | formalParams == [] = Just Map.empty
                     | otherwise = gatherInputs =<< maybeInputs
        gatherInputs actualParams = foldM f Map.empty formalParams
          where f acc ident = do
                  val <- Map.lookup ident actualParams
                  guard $ True -- test right kind of argument later
                  return $ Map.insert ident val acc
        gatherOutputs (SysState finalState) =
          map (\i -> (i, finalState Map.! i)) $ p_outputs prog


