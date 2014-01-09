module Patterns where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List

import Circuit

row :: (Applicative m, Monad m) => ((a, b) -> m (c, a)) -> ((a, [b]) -> m ([c], a))
row _   (x, []  ) = return ([], x)
row cir (x, y:ys) = do (z, x') <- cir (x, y)
                       first (z:) <$> row cir (x', ys)

bigMux :: (Circuit m s) => s -> [s] -> [s] -> m [s]
bigMux a b c = zipWithM (\y z -> mux3 (a,y,z)) b c

doubleMux :: (SequentialCircuit m s) => [s] -> s -> s -> s -> s -> m  s
doubleMux [a,b] x0 x1 x2 x3 =
    do  m1 <- mux3 (a,x0,x1)  
        m2 <- mux3 (a,x2,x3)
        mux3(b,m1,m2)

bigDoubleMux :: (SequentialCircuit m s) => [s] -> [s] -> [s] -> [s] -> [s] -> m [s]
bigDoubleMux [a,b] x0 x1 x2 x3 = 
   sequence $ zipWith4 (doubleMux [a,b]) x0 x1 x2 x3

