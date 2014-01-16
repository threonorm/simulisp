{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Caillou.NetlistGen where

import Control.Applicative
import Control.Arrow
import Control.Monad.Fix
import Control.Monad.State
import qualified Data.Map as Map

import Netlist.AST
import Caillou.Circuit


-- Netlist generation

newtype NetlistGen a = NetlistGen (State NetlistGenState a)
                     deriving (Functor, Applicative, Monad, MonadFix,
                               MonadState NetlistGenState)

-- we could use Writer instead of State for the equation list
data NetlistGenState = NGS { ngsCtr  :: Int
                           , ngsEqs  :: [Equation]
                           , ngsVars :: Environment Ty
                           }

gensym :: NetlistGen String
gensym = do n <- gets ngsCtr
            modify (\s -> s { ngsCtr = n + 1 })
            return ("_l_" ++ show n)

makeWireWithExpr :: Exp -> NetlistGen Arg
makeWireWithExpr = makeWhateverWithExpr TBit

-- Generally, any Arg value which is taken as input or returned as output
-- by any circuit represents a single wire.
-- This is enforced by encapsulation, not at the type-system level, since Args
-- in the netlist language can be sheets of wires.
-- The function below breaks this invariant: it is only for use in
-- the definition of accessROM/accessRAM.

makeWhateverWithExpr :: Ty -> Exp -> NetlistGen Arg
makeWhateverWithExpr ty expr = do
  sym <- gensym
  -- would be nicer with the Lens library
  modify (\s -> s { ngsEqs  = (sym, expr) : (ngsEqs s)
                  , ngsVars = Map.insert sym ty $ ngsVars s })
  return $ Avar sym

mkBinopGate :: Binop -> (Arg, Arg) -> NetlistGen Arg
mkBinopGate binop = \(a, b) -> makeWireWithExpr $ Ebinop binop a b

instance Circuit NetlistGen Arg where
  -- one = makeWireWithExpr $ Earg (Aconst (VBit True)) 
  -- zero = makeWireWithExpr $ Earg (Aconst (VBit False))
  zero = return . Aconst . VBit $ False
  one  = return . Aconst . VBit $ True
  neg a = makeWireWithExpr $ Enot a
  and2  = mkBinopGate And
  or2   = mkBinopGate Or
  xor2  = mkBinopGate Xor
  nand2 = mkBinopGate Nand
  mux3 (s, a, b) = makeWireWithExpr $ Emux s a b

instance SequentialCircuit NetlistGen Arg where
  delay a = makeWireWithExpr (ereg a)
    where ereg (Avar v) = Ereg v
          ereg (Aconst c) = Earg (Aconst c)

instance MemoryCircuit NetlistGen Arg where
  accessROM arrName addrSize wordSize readAddrList = do
    readAddrArr <- listToArr readAddrList
    modify (\s -> s { ngsEqs  = (arrName, Erom addrSize wordSize readAddrArr)
                                : (ngsEqs s)
                    , ngsVars = Map.insert arrName (TBitArray wordSize)
                                $ ngsVars s
                    })
    arrToList wordSize (Avar arrName)

  accessRAM addrSize wordSize (readAddrList, writeEnable, writeAddrList, writeDataList) = do
    readAddrArr  <- listToArr readAddrList
    writeAddrArr <- listToArr writeAddrList
    writeDataArr <- listToArr writeDataList
    wordArr <- makeWhateverWithExpr (TBitArray wordSize)
               $ Eram addrSize wordSize readAddrArr
                      writeEnable writeAddrArr writeDataArr
    arrToList wordSize wordArr
  
    
listToArr :: [Arg] -> NetlistGen Arg
listToArr []     = error "listToArr: Empty list"
listToArr (x:xs) = foldM addWire x $ zip [2..] xs
  where addWire ar (prefixSize, wire) =
          makeWhateverWithExpr (TBitArray prefixSize) $ Econcat ar wire

arrToList :: Int -> Arg -> NetlistGen [Arg]
arrToList size ar = mapM select [0..(size - 1)]
  where select i = makeWireWithExpr $ Eselect i ar

-- Beware! the final map of variables doesn't contain the inputs
synthesizeBarebonesNetlist :: (a -> NetlistGen b) -> a
                           -> (b, [Equation], Environment Ty)
synthesizeBarebonesNetlist circuit inputs =
  (out, ngsEqs finalState, ngsVars finalState)
  where (out, finalState) = runState comp initState
        (NetlistGen comp) = circuit inputs
        initState = NGS { ngsCtr = 0, ngsEqs = [], ngsVars = Map.empty }

-- Setting up the inputs and outputs is slightly awkward,
-- but this is done only once
-- Of course, we expect that both a and b
-- are composite types built over the base type Arg

-- example:
-- synthesizeNetlistAST halfAdder (Avar "a", Avar "b")
--                      ["a", "b"] (\(s, r) -> [("s", s), ("r", r)]) 
          
synthesizeNetlistAST :: (a -> NetlistGen b)   -- Circuit description written in the EDSL
                     -> a                     -- Input variables
                     -> [Ident]               -- List of variables which appear as inputs
                     -> (b -> [(Ident, Arg)]) -- Explicit assignment of names to outputs
                     -> Program
synthesizeNetlistAST circuit inputs inputVarList outputVarFn =
  Pr { p_inputs  = inputVarList
     , p_outputs = outputVarList
     , p_vars    = vars `Map.union` externalVars
     , p_eqs     = eqs ++ outputPluggingEqs
     }
  where (out, eqs, vars) = synthesizeBarebonesNetlist circuit inputs
        externalVars = Map.fromList $ zip (inputVarList ++ outputVarList) (repeat TBit)
        outputPlugging = outputVarFn out
        outputVarList = map fst outputPlugging
        outputPluggingEqs = map (second Earg) outputPlugging
