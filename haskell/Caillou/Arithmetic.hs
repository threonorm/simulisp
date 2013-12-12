module Arithmetic where

import Control.Applicative

import Circuit
import Patterns

halfAdd :: (Circuit m s) => (s,s) -> m (s,s)
halfAdd (a, b) = (,) <$> (a -^^- b) <*> (a -&&- b)

fullAdd :: (Circuit m s) => (s,s,s) -> m (s,s)
fullAdd (a, b, c) = do (s1, r1) <- halfAdd (a, b)
                       (s2, r2) <- halfAdd (s1, c)
                       r <- r1 -^^- r2
                       return (s2, r)
                       
adder :: (Circuit m s) => (s, [(s,s)]) -> m ([s], s)
adder = row fullAdd'
  where fullAdd' (c, (a, b)) = fullAdd (c, a, b)

