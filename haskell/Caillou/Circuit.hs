{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- TODO : add lots of comments to make the code perfectly clear to the uninitiated

module Circuit where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Fix

pairA :: (Applicative f) => (f a, f b) -> f (a, b)
pairA (a, b) = (,) <$> a <*> b

-- Require Applicative for convenience,
-- since it is not a superclass of Monad by historical accident
class (Applicative m, Monad m) => Circuit m s where
  neg   :: s     -> m s
  -- uncurried functions in traditional Lava style
  and2  :: (s,s) -> m s
  or2   :: (s,s) -> m s
  xor2  :: (s,s) -> m s
  nand2 :: (s,s) -> m s
  nor2  :: (s,s) -> m s
  -- default implementations for xor, etc.
  xor2 (a,b) = and2 =<< pairA (or2 (a,b), neg =<< and2 (a, b))
  nand2 = neg <=< and2
  nor2  = neg <=< or2

(<&&>), (<||>), (<^^>), (<~&>), (<~|>) :: (Circuit m s) => s -> s -> m s
a <&&> b = and2  (a, b)
a <||> b = or2   (a, b)
a <^^> b = xor2  (a, b)
a <~&> b = nand2 (a, b)
a <~|> b = nor2  (a, b)

  
  

-- monadic fixpoint = loop in circuit
class (Circuit m s, MonadFix m) => Sequential m s where
  delay :: m s -- Register

newtype SimulateBoolFn a = SBF (Identity a)
                         deriving (Functor, Applicative, Monad, MonadFix)

instance Circuit SimulateBoolFn Bool where
  neg   = return . not
  and2  = return . uncurry (&&)
  or2   = return . uncurry (||)

-- perhaps use the more restrictive
-- simulateCircuit :: (forall m s . (Circuit m s) => a -> m b) -> a -> b
simulateCircuit :: (a -> SimulateBoolFn b) -> a -> b
simulateCircuit c x = let (SBF y) = c x in runIdentity y

halfAdd :: (Circuit m s) => (s,s) -> m (s,s)
halfAdd (a, b) = do s <- a <^^> b
                    r <- a <&&> b
                    return (s, r)

