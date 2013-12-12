{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Simulation where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

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
-- simulateBoolFn :: (forall m s . (Circuit m s) => a -> m b) -> a -> b
simulateBoolFn :: (a -> SimulateBoolFn b) -> a -> b
simulateBoolFn c x = let (SBF y) = c x in runIdentity y

-- Sequential simulation on lists of input values

-- Strategy: the order of the execution of the "delay" commands is deterministic
-- Therefore, if we feed the registers in the same order we read their new values,
-- it will be consistent.

newtype SimulateSeq a = SS (State SimSeqState a)
                      deriving (Functor, Applicative, Monad, MonadFix,
                                MonadState SimSeqState)

data SimSeqState = SSS { oldRegQueue :: [Bool]
                       , newRegStack :: [Bool] }

runSS :: SimulateSeq a -> SimSeqState -> (a, SimSeqState)
runSS (SS x) = runState x


instance Circuit SimulateSeq Bool where
  neg   = return . not
  and2  = return . uncurry (&&)
  or2   = return . uncurry (||)
  mux3 (s, a, b) = return $ if s then a else b

instance SequentialCircuit SimulateSeq Bool where
  delay newThis = do
    old <- gets oldRegQueue
    new <- gets newRegStack
    let (oldThis:next) = old
    put $ SSS { oldRegQueue = next, newRegStack = newThis:new }
    return oldThis
  
simulateSeq :: (a -> SimulateSeq b) -> [a] -> [b]
simulateSeq circuit = go (repeat False)
  where go _    []           = []
        go regQueue (input:next) =
          let (output, newSSS) =
                runSS (circuit input) $ SSS { oldRegQueue = regQueue,
                                              newRegStack = [] }
              newRegQueue = reverse . newRegStack $ newSSS in
          output : go newRegQueue next

