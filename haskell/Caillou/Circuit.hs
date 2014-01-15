{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- TODO : add lots of comments to make the code perfectly clear to the uninitiated

module Caillou.Circuit where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix

-- TODO : reorganize file layout to remove redundancies
-- there are currently 2 copies of NetlistAST

-- Require Applicative for convenience,
-- since it is not a superclass of Monad by historical accident
class (Applicative m, Monad m) => Circuit m s | m -> s where
  zero :: m s
  one :: m s
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


-- Monadic fixpoint = loop in circuit
class (Circuit m s, MonadFix m) => SequentialCircuit m s where
  delay :: s -> m s -- Register

-- This class is perfectly modeled after the target netlist language
class (SequentialCircuit m s) => MemoryCircuit m s where
  -- addr size, word size, read addr
  accessROM :: Int -> Int -> [s] -> m [s]
  -- addr size, word size, read addr, write enable, write addr, write data
  accessRAM :: Int -> Int -> ([s], s, [s], [s]) -> m [s]


