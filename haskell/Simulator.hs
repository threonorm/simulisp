module Simulator where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Bool

import NetlistAST

type WireState = Environment Value
data Memory = Mem { registers :: Environment Value
                  , ram :: IntMap Bool
                  , rom :: Array Int Bool -- immutable array
                  }
data SysState = SysState { wireState :: WireState
                         , memory :: Memory }
type Outputs = [(Ident, Value)]


extractArg :: WireState -> Arg -> Value
extractArg st (Avar i) = st Map.! i 
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


-- Partial function, doesn't handle memory
-- TODO (in the distant future): improve this with generic programming / SYB stuff
compute :: WireState -> Exp -> Value
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


valueToInt :: Value -> Int
valueToInt (VBit b) = if b then 1 else 0
valueToInt (VBitArray bs) =
  foldl' (\acc digit -> acc*10+digit) 0 . map (valueToInt . VBit) . reverse $ bs

simulationStep :: SysState -> Equation -> SysState

simulationStep st (_, Ereg _) = st -- do nothing

-- TODO: do something with addrSize?
simulationStep st (ident, Erom addrSize wordSize readAddr) =
  setWire st ident result
  where result | wordSize == 1 = VBit . getROMBit $ wordAddr
               | otherwise = VBitArray . map getROMBit
                             $ [wordAddr..(wordAddr + wordSize - 1)]
        wordAddr = valueToInt $ extractArg (wireState st) readAddr
        getROMBit bitAddr
          | bitAddr < addrMin || bitAddr > addrMax = False
          | otherwise = romArray Array.! bitAddr
        (addrMin, addrMax) = Array.bounds romArray
        romArray = rom (memory st)


simulationStep st (ident, Eram addrSize wordSize readAddr
                               writeEnable writeAddr writeData) =
  undefined -- thomas, tu te charges de ça !
            -- il faudra modifier la RAM à la fin du cycle comme pour les registres

simulationStep st (ident, expr) =
  setWire st ident $ compute (wireState st) expr

setWire :: SysState -> Ident -> Value -> SysState
setWire st ident val =
  st { wireState = Map.insert ident val (wireState st) }

-- TODO: use reader monad for memory

simulateCycle :: Program -> Memory -> WireState -- old memory + inputs
              -> (Memory, Outputs)
simulateCycle prog oldMem inputWires =
  (newMem &&& gatherOutputs)
  . wireState . foldl' simulationStep initialState
  $ p_eqs prog
  where initialState = SysState { wireState = initialWires,
                                  memory = oldMem }
        initialWires = inputWires `Map.union` registers oldMem
        gatherOutputs finalWires =
          map (\i -> (i, finalWires Map.! i)) $ p_outputs prog
        newMem finalWires = oldMem { registers = Map.fromList . map getNewVal
                                                 $ programRegisters prog }
          where getNewVal = second (finalWires Map.!)


initialWireState :: Program -- Sorted netlist
                 -> Environment Value -- Map of inputs
                 -> Maybe WireState -- outputs with possibility of error
                                    -- TODO: refine error signaling
initialWireState prog actualParams = foldM f Map.empty formalParams
  where formalParams = p_inputs prog
        f acc ident = do
          val <- Map.lookup ident actualParams
          guard $ True -- TODO: test right kind of argument
          return $ Map.insert ident val acc

iteratedSimulation :: Program
                   -> Maybe [WireState]
                   -> Maybe (Array Int Bool)
                   -> [[(Ident, Value)]]
iteratedSimulation prog maybeInputs maybeROM =
  -- Two cases:
  -- * if we have no input: simulate infinitely (with laziness)
  -- * if we're given a list of inputs: we simulate until the inputs run out
  -- TODO: ensure in the first case we have no inputs
  let inputWires = maybe (repeat Map.empty) id maybeInputs
      initialMemory = Mem { registers = initRegs,
                            ram = IntMap.empty,
                            rom = maybe (Array.array (0,-1) []) id maybeROM }
  in trace (simulateCycle prog) initialMemory inputWires
  
  where initRegs = Map.fromList . flip zip (repeat (VBit False)) $ regs
        regs = map fst . filter isReg $ p_eqs prog
        isReg (_, (Ereg _)) = True
        isReg _             = False
        

programRegisters :: Program -> [(Ident,Ident)]
programRegisters = catMaybes . map p . p_eqs
  where p (x, (Ereg y)) = Just (x,y)
        p _             = Nothing
        

-- Custom list builder function, not quite zipWith or scanl
-- trace f a0 [x0, ...] = [y1, ...]
-- where (a1, y1) = f a0 x0
--       (a2, y2) = f a1 x1
--       etc. 
trace :: (a -> b -> (a, c)) -> a -> [b] -> [c]
trace _ _  []      = []
trace f a0 (x0:xs) = let (a1, y1) = f a0 x0
                     in y1 : trace f a1 xs
