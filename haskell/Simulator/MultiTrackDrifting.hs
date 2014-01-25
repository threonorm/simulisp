{-# LANGUAGE ParallelListComp, TupleSections #-}

module Simulator.MultiTrackDrifting (awesomeSimulation) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Either
import Data.Maybe
import Data.Foldable (sequenceA_)
import Data.Traversable (sequenceA)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.ST.Strict
import qualified Control.Monad.ST.Lazy as LazyST
import Data.STRef
import Control.Monad.Primitive
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import qualified Debug.Trace as DT

import Netlist.AST
import Util.ListUtil
import Util.BinUtils

type LazyST = LazyST.ST


-- First step: Symbolic execution
-- Mostly copied and adapted from Simulator.Simulator 

newtype Symbol = Symbol Int

data SymBit = SVar Symbol
            | SConst Bool
            | SROMBit Ident Int

data SymValue = SBit SymBit
              | SBitArray [SymBit]

symValToList :: SymValue -> [SymBit]
symValToList (SBit b) = [b]
symValToList (SBitArray bs) = bs

type WireState = Environment SymValue -- the name comes from Simulator.Simulator

data SymExpr = Snot SymBit
             | Sbinop Binop SymBit SymBit
             | Smux SymBit SymBit SymBit
             | Sgetram Ident Int
             -- sgetreg is handled specially

data SymProg = SymProg { spInstructions :: [SymInstruction]
                       , spROMs :: Map Ident SROM
                       , spRAMs :: Map Ident SRAM
                       , spRegs :: [SymBit] -- variables which become the *new*
                                            -- values of the registers
                       , spNumVars :: Int
                       , spOutputs :: [(Ident, SymValue)] }
               
-- addrSize, wordSize, readAddr
data SROM = SROM Int Int [SymBit]
-- addrSize, wordSize, readAddr, writeEnable, writeAddr, writeData
data SRAM = SRAM Int Int [SymBit] SymBit [SymBit] [SymBit]
               
data SymInstruction = SAssignVar Symbol SymExpr
                    | SComputeROMReadAddr Ident
                    | SComputeRAMReadAddr Ident

symbolicExecution :: Program -> SymProg
symbolicExecution program =
  let (regEqs, otherEqs) = separateRegs $ p_eqs program
      (regWires, regSources, numRegs) = allocVarsOfRegs regEqs
      (finalWires, instructionStack, numVars) =
        foldl' (flip symExecStep) (regWires, [], numRegs) otherEqs
        
      identSymValue = (finalWires Map.!)
      argSymBit = extractBit finalWires
      argSymBits = symValueToList . extractArg finalWires
      fO (ident, Erom addrSize wordSize readAddr) =
        Just (ident, SROM addrSize wordSize (argSymBits readAddr))
      fO _ = Nothing
      fA (ident, Eram as ws ra we wa dat) =
        Just (ident, SRAM as ws (argSymBits ra) (argSymBit we)
                          (argSymBits wa) (argSymBits dat))
      fA _ = Nothing
  in
   SymProg { spInstructions = reverse instructionStack
           , spROMs = extract fO otherEqs
           , spRAMs = extract fA otherEqs
           , spRegs = map (toBit . identSymValue) regSources
           , spNumVars = numVars
           , spOutputs = map (id &&& identSymValue) $ p_outputs program
           }
  where separateRegs = partitionEithers . map f
          where f (ident, Ereg source) = Left (ident, source)
                f equation             = Right equation
        allocVarsOfRegs regs = (wires, sources, length regs)
          where (idents, sources) = unzip regs
                wires = Map.fromList [ (x, SBit (SVar (Symbol i)))
                                     | x <- idents | i <- [0..] ]
        extract f = Map.fromList . mapMaybe f

                        
symExecStep :: Equation
            -> (WireState, [SymInstruction], Int)
            -> (WireState, [SymInstruction], Int)
symExecStep (ident, expr) (wires, instructions, numVars) = case expr of

  -- Section 1: gates which deal with memory
  
  (Ereg _) -> error "registers should have been eliminated by now"
  
  (Eram addrSize wordSize readAddr writeEnable writeAddr writeData) ->
    let next = numVars + wordSize
        foo = [ (Symbol (numVars + i), Sgetram ident i) | i <- [0..(wordSize-1)] ]
        wire = SBitArray . map (SVar . fst) $ foo
        assignVarsOfRam = map (\(v,instr) -> SAssignVar v instr) foo
    in 
     (addWire wire,
      reverse assignVarsOfRam ++ (SComputeRAMReadAddr ident : instructions),
      next)
          
  (Erom addrSize wordSize readAddr) ->
    let wire = SBitArray [ SROMBit ident i | i <- [0..wordSize] ]
    in 
     (addWire wire,
      SComputeROMReadAddr ident : instructions,
      numVars)

  -- Section 2: stupid shuffling around of data

  (Earg a) -> (addWire (extractArg wires a),
               instructions,
               numVars)
               
  (Eselect i a) -> let bit = (symValueToList $ extractArg wires a) !! i in
    (addWire (SBit bit), instructions, numVars)

  (Eslice i1 i2 a) -> let arr = take (i2 - i1 + 1) . drop i1 . symValueToList
                                $ extractArg wires a in
                      (addWire (SBitArray arr), instructions, numVars)
                      
  (Econcat a1 a2) -> let arr = symValueToList vA1 ++ symValueToList vA2
                         vA1 = extractArg wires a1
                         vA2 = extractArg wires a2 in
                     (addWire (SBitArray arr), instructions, numVars)

  -- Section 3: actual computation with combinational logic gates

  _ -> let sexpr = symCompute wires expr
           this = Symbol numVars
           next = numVars + 1
       in (addWire (SBit (SVar this)),
           SAssignVar this sexpr : instructions,
           next)

  where addWire x = Map.insert ident x wires


symCompute :: WireState -> Exp -> SymExpr
symCompute st (Enot a) =
  Snot (extractBit st a)
symCompute st (Ebinop op a1 a2) =
  Sbinop op (extractBit st a1) (extractBit st a2)
symCompute st (Emux a1 a2 a3) = 
  Smux (extractBit st a1) (extractBit st a2) (extractBit st a3)

extractBit :: WireState -> Arg -> SymBit
extractBit st (Avar i) = toBit $ st Map.! i 
extractBit _  (Aconst (VBit v)) = SConst v

toBit :: SymValue -> SymBit
toBit (SBit b) = b
toBit (SBitArray [b]) = b

extractArg :: WireState -> Arg -> SymValue
extractArg st (Avar i) = st Map.! i 
extractArg _  (Aconst (VBit v)) = SBit (SConst v)
extractArg _  (Aconst (VBitArray vs)) = SBitArray $ map SConst vs

symValueToList :: SymValue -> [SymBit]
symValueToList (SBit b) = [b]
symValueToList (SBitArray bs) = bs


-- Phase 2: convert the symbolic program into a procedure operating on mutable arrays

type BoolVect s = MV.STVector s Bool

data MutableData s = MD { varVect :: BoolVect s
                        , regVect :: BoolVect s
                        , ramVects :: Map Ident (BoolVect s)
                        , ramReadAddrs :: Map Ident (STRef s Int)
                        , romReadAddrs :: Map Ident (STRef s Int)
                        }

initMutableData :: SymProg -> ST s (MutableData s)
initMutableData program = do
  regv <- MV.replicate (length (spRegs program)) False
  let f (SRAM addrSize wordSize _ _ _ _) = MV.replicate ((2^addrSize) * wordSize) False
  ramv <- sequenceA . Map.map f $ spRAMs program
  varv <- MV.replicate (spNumVars program) False
  ramras <- sequenceA . Map.map (const $ newSTRef 0) $ spRAMs program
  romras <- sequenceA . Map.map (const $ newSTRef 0) $ spROMs program
  return $ MD { regVect = regv, ramVects = ramv, varVect = varv
              , ramReadAddrs = ramras, romReadAddrs = romras }


readBit :: MutableData s -> Map Ident (Vector Bool) -> SymBit -> ST s Bool
readBit md _ (SVar (Symbol x)) = varVect md `MV.read` x
readBit _  _ (SConst c) = return c
readBit md romVects (SROMBit ident offset) =
  let romVect = romVects Map.! ident
      addrRef = romReadAddrs md Map.! ident in
  (\addr -> romVect !!? (addr + offset)) <$> readSTRef addrRef

compileSymProgram :: (Map Ident (Vector Bool)) -> SymProg
                  -> ST s (MutableData s, ST s ())
compileSymProgram romVects program = do
  md <- initMutableData program

  let getValue = readBit md romVects
    
      numRegs = MV.length $ regVect md
      varsOfRegs = MV.slice 0 numRegs $ varVect md
      initCycle = varsOfRegs `MV.copy` regVect md

      executeInstructions = mapM_ execInstr $ spInstructions program
      execInstr (SAssignVar (Symbol dest) expr) =
        MV.write (varVect md) dest =<< compute expr
      execInstr (SComputeROMReadAddr ident) =
        let (SROM _ wordSize readAddr) = spROMs program Map.! ident
            addrRef = romReadAddrs md Map.! ident in
        writeSTRef addrRef . (* wordSize) =<< computeAddr readAddr
      execInstr (SComputeRAMReadAddr ident) =
        let (SRAM _ wordSize readAddr _ _ _) = spRAMs program Map.! ident
            addrRef = ramReadAddrs md Map.! ident in
        writeSTRef addrRef . (* wordSize) =<< computeAddr readAddr

      -- does this precompute (eliminate the list) correctly? hope so...
      computeAddr = foldM f 0 . reverse -- careful with the order of bits!
        where f acc x = do y <- getValue x
                           return $! if y then 2*acc + 1 else 2*acc

      compute (Snot x) = not <$> getValue x
      compute (Sbinop op x y) = binopFn op <$> getValue x <*> getValue y
      compute (Smux a b c) = f <$> getValue a <*> getValue b <*> getValue c
        where f x y z = (not x && y) || (x && z)
      compute (Sgetram ident offset) = 
        let ramVect = ramVects md Map.! ident
            addrRef = ramReadAddrs md Map.! ident in
        (\addr -> ramVect `MV.read` (addr + offset)) =<< readSTRef addrRef

      endCycle = sequence_ regUpdates >> sequenceA_ ramUpdates
      regUpdates = zipWith (\i j -> MV.write (regVect md) i =<< getValue j)
                           [0..] (spRegs program)
      ramUpdates = Map.mapWithKey f (spRAMs program)
        where f ident (SRAM _ wordSize _ writeEnable writeAddr writeData) =
                let getAddr = (* wordSize) <$> computeAddr writeAddr
                    ramVect = ramVects md Map.! ident
                    writeToAddr addr =
                      sequence_ [ MV.write ramVect (addr+i) =<< getValue x
                                | i <- [0..]
                                | x <- writeData ]
                in do
                  we <- getValue writeEnable
                  when we $ writeToAddr =<< getAddr
                  
  return (md, initCycle >> executeInstructions >> endCycle)

-- a ROM array contains only the data given by the user which may be not enough
-- -> padding with zeroes at the end
-- does this kind of bounds check hinder performance?
(!!?) :: V.Vector Bool -> Int -> Bool
vect !!? ix = case vect V.!? ix of
  Nothing -> False
  Just b -> b


-- Phase 3: glue everything together

awesomeSimulation :: Program -> Maybe (Environment [Bool]) -> [[(Ident, Value)]]
awesomeSimulation program maybeROMs =
  let symprog = symbolicExecution program
      roms = maybe Map.empty (Map.map V.fromList) maybeROMs
      doStrict = LazyST.strictToLazyST
  in
   LazyST.runST $ do
     (mutableData, compiledProgram) <- doStrict $ compileSymProgram roms symprog
     let extractOutputs = mapM f $ spOutputs symprog
           where f (ident, symval) = (ident,) . VBit <$>
                                     readBit mutableData roms (toBit symval)
         loop = do doStrict compiledProgram
                   x <- doStrict extractOutputs
                   xs <- loop
                   return (x:xs)
     loop


