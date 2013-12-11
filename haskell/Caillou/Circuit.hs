{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo #-}

-- TODO : add lots of comments to make the code perfectly clear to the uninitiated

module Circuit where

import Control.Applicative
import Control.Arrow
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as Map

-- TODO : reorganize file layout to remove redundancies
-- there are currently 2 copies of NetlistAST
import NetlistAST

-- Require Applicative for convenience,
-- since it is not a superclass of Monad by historical accident
class (Applicative m, Monad m) => Circuit m s where
  neg  :: s     -> m s
  mux3 :: (s, s, s) -> m s
  -- uncurried functions in traditional Lava style
  and2  :: (s,s) -> m s
  or2   :: (s,s) -> m s
  xor2  :: (s,s) -> m s
  nand2 :: (s,s) -> m s
  nor2  :: (s,s) -> m s
  
  -- default implementations for xor, etc.
  neg a = nand2 (a, a)
  xor2 (a,b) = (a -||- b) <&&> (neg =<< (a -&&- b))
  nand2 = neg <=< and2
  nor2  = neg <=< or2
  mux3 (s, a, b) = (s -&&- a) <||> (neg s <&&- b)

(-&&-), (-||-), (-^^-), (-~&-), (-~|-) :: (Circuit m s) => s -> s -> m s
a -&&- b = and2  (a, b)
a -||- b = or2   (a, b)
a -^^- b = xor2  (a, b)
a -~&- b = nand2 (a, b)
a -~|- b = nor2  (a, b)

-- Angle brackets suggest effects

(<&&-), (<||-), (<^^-), (<~&-), (<~|-) :: (Circuit m s) => m s -> s -> m s
a <&&- b = join $ (-&&-) <$> a <*> pure b
a <||- b = join $ (-||-) <$> a <*> pure b
a <^^- b = join $ (-^^-) <$> a <*> pure b
a <~&- b = join $ (-~&-) <$> a <*> pure b
a <~|- b = join $ (-~|-) <$> a <*> pure b

(-&&>), (-||>), (-^^>), (-~&>), (-~|>) :: (Circuit m s) => s -> m s -> m s
a -&&> b = join $ (-&&-) <$> pure a <*> b
a -||> b = join $ (-||-) <$> pure a <*> b
a -^^> b = join $ (-^^-) <$> pure a <*> b
a -~&> b = join $ (-~&-) <$> pure a <*> b
a -~|> b = join $ (-~|-) <$> pure a <*> b

(<&&>), (<||>), (<^^>), (<~&>), (<~|>) :: (Circuit m s) => m s -> m s -> m s
a <&&> b = join $ (-&&-) <$> a <*> b
a <||> b = join $ (-||-) <$> a <*> b
a <^^> b = join $ (-^^-) <$> a <*> b
a <~&> b = join $ (-~&-) <$> a <*> b
a <~|> b = join $ (-~|-) <$> a <*> b


-- monadic fixpoint = loop in circuit
class (Circuit m s, MonadFix m) => Sequential m s where
  delay :: s -> m s -- Register

-- Add ROM/RAM class

-- Simple simulation of combinational circuits

newtype SimulateBoolFn a = SBF (Identity a)
                         deriving (Functor, Applicative, Monad, MonadFix)

instance Circuit SimulateBoolFn Bool where
  neg   = return . not
  and2  = return . uncurry (&&)
  or2   = return . uncurry (||)
  mux3 (s, a, b) = return $ if s then a else b

-- perhaps use the more restrictive
-- simulateCircuit :: (forall m s . (Circuit m s) => a -> m b) -> a -> b
simulateCircuit :: (a -> SimulateBoolFn b) -> a -> b
simulateCircuit c x = let (SBF y) = c x in runIdentity y


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
makeWireWithExpr expr = do
  sym <- gensym
  -- would be nicer with the Lens library
  modify (\s -> s { ngsEqs  = (sym, expr) : (ngsEqs s)
                  , ngsVars = Map.insert sym TBit $ ngsVars s })
  return $ Avar sym

mkBinopGate :: Binop -> (Arg, Arg) -> NetlistGen Arg
mkBinopGate binop = \(a, b) -> makeWireWithExpr (Ebinop binop a b)

instance Circuit NetlistGen Arg where
  neg a = makeWireWithExpr (Enot a)
  and2  = mkBinopGate And
  or2   = mkBinopGate Or
  xor2  = mkBinopGate Xor
  nand2 = mkBinopGate Nand

instance Sequential NetlistGen Arg where
  delay a = makeWireWithExpr (ereg a)
    where ereg (Avar v) = Ereg v
          ereg (Aconst c) = Earg (Aconst c)


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
      

-- Test

halfAdd :: (Circuit m s) => (s,s) -> m (s,s)
halfAdd (a, b) = (,) <$> (a -^^- b) <*> (a -&&- b)

andLoop :: (Sequential m s) => s -> m s
andLoop inp = do rec out <- inp -&&- mem
                     mem <- delay out
                 return out
                 

