{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo #-}

module Simulation where

import Control.Applicative
import Control.Monad.Identity

import Circuit


-- Simple simulation of combinational circuits

newtype SimulateBoolFn a = SBF (Identity a)
                         deriving (Functor, Applicative, Monad)

instance Circuit SimulateBoolFn Bool where
  neg   = return . not
  and2  = return . uncurry (&&)
  or2   = return . uncurry (||)
  mux3 (s, a, b) = return $ if s then a else b

-- perhaps use the more restrictive
-- simulateCircuit :: (forall m s . (Circuit m s) => a -> m b) -> a -> b
simulateCircuit :: (a -> SimulateBoolFn b) -> a -> b
simulateCircuit c x = let (SBF y) = c x in runIdentity y


