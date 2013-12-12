module Patterns where

import Control.Arrow
import Control.Applicative
import Control.Monad

import Circuit

row :: (Applicative m, Monad m) => ((a, b) -> m (c, a)) -> ((a, [b]) -> m ([c], a))
row _   (x, []  ) = return ([], x)
row cir (x, y:ys) = do (z, x') <- cir (x, y)
                       first (z:) <$> row cir (x', ys)

