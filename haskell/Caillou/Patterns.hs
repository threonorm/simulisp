module Caillou.Patterns where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List

import Caillou.Circuit

row :: (Applicative m, Monad m) => ((a, b) -> m (c, a)) -> ((a, [b]) -> m ([c], a))
row _   (x, []  ) = return ([], x)
row cir (x, y:ys) = do (z, x') <- cir (x, y)
                       first (z:) <$> row cir (x', ys)

dichotomicFold :: (Circuit m s) => ((s, s) -> m s) -> [s] -> m s
dichotomicFold _ []  = error "dichotomicFold: empty list"
dichotomicFold _ [x] = return x
dichotomicFold f xs  = dichotomicFold f =<< pass xs
  where pass []        = return []
        pass [y]       = return [y]
        pass (y:y':ys) = (:) <$> f (y, y') <*> pass ys

bigMux :: (Circuit m s) => s -> [s] -> [s] -> m [s]
bigMux a b c = zipWithM (\y z -> mux3 (a,y,z)) b c

doubleMux :: (Circuit m s) => [s] -> s -> s -> s -> s -> m  s
doubleMux [a,b] x0 x1 x2 x3 =
    do m1 <- mux3 (a,x0,x1)  
       m2 <- mux3 (a,x2,x3)
       mux3 (b,m1,m2)

bigDoubleMux :: (Circuit m s) => [s] -> [s] -> [s] -> [s] -> [s] -> m [s]
bigDoubleMux [a,b] x0 x1 x2 x3 = 
   sequence $ zipWith4 (doubleMux [a,b]) x0 x1 x2 x3

bigDelay :: (SequentialCircuit m s) => [s] -> m [s]
bigDelay = mapM delay

-- It's dangerous to use mapM in a cycle!
-- Functions such as mapMn generate a list whose spine depends only
-- on an integer given as argument, so that the definitions of two
-- such lists can depend on one another lazily without issues
-- This uses irrefutable *lazy* patterns ~(x:xs)

mapn :: (Integral i) => i -> (a -> b) -> [a] -> [b]
mapn 0 _ _ = []
mapn n f ~(x:xs) = f x : mapn (n-1) f xs

mapMn :: (Integral i, Monad m) => i -> (a -> m b) -> [a] -> m [b]
mapMn n f xs = sequence $ mapn n f xs

zipWithn :: (Integral i) => i -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithn 0 _ _ _ = []
zipWithn n f ~(x:xs) ~(y:ys) = f x y : zipWithn (n-1) f xs ys

zipWithMn :: (Integral i, Monad m) => i -> (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithMn n f xs ys = sequence $ zipWithn n f xs ys

bigMuxn :: (Circuit m s, Integral i) => i -> s -> [s] -> [s] -> m [s]
bigMuxn n a b c = zipWithMn n (\y z -> mux3 (a, y, z)) b c

bigDelayn :: (SequentialCircuit m s, Integral i) => i -> [s] -> m [s]
bigDelayn n = mapMn n delay

takeForce :: Int -> [a] -> [a]
takeForce 0 _ = []
takeForce n ~(x:xs) = x : takeForce (n-1) xs

