{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

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
  delay :: m s -- Register


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

addEquation :: Equation -> NetlistGen ()
addEquation e = modify (\s -> s { ngsEqs = e : (ngsEqs s) })

makeWireWithExpr :: Exp -> NetlistGen Arg
makeWireWithExpr expr = do
  sym <- gensym
  addEquation (sym, expr)
  return $ Avar sym

-- TODO : handle vars map !!!
-- TODO : abstract this pattern with the gensym
mkBinopGate :: Binop -> (Arg, Arg) -> NetlistGen Arg
mkBinopGate binop = \(a, b) -> makeWireWithExpr (Ebinop binop a b)


instance Circuit NetlistGen Arg where
  neg a = makeWireWithExpr (Enot a)
  and2  = mkBinopGate And
  or2   = mkBinopGate Or
  xor2  = mkBinopGate Xor
  nand2 = mkBinopGate Nand


-- TODO : allow setting output names
synthesizeNetlistAST circuit inputs =
  let (NetlistGen comp) = circuit inputs in
  (ngsEqs &&& ngsVars) . flip execState initState $ comp
  where
    initState = NGS { ngsCtr = 0, ngsEqs = [], ngsVars = Map.empty }
  

-- Test

halfAdd :: (Circuit m s) => (s,s) -> m (s,s)
halfAdd (a, b) = (,) <$> (a -^^- b) <*> (a -&&- b)



