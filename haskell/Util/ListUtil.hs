-- List combinators

module Util.ListUtil where

-- for some reason this is not in the standard libraries
unintersperse :: (Eq a) => a -> [a] -> [[a]]
unintersperse x xs = let (y, rest) = break (== x) xs
                     in y : case rest of
                       []    -> []
                       (_:q) -> unintersperse x q

-- Custom list builder function, not quite zipWith or scanl
-- trace f a0 [x0, ...] = [y1, ...]
-- where (a1, y1) = f a0 x0
--       (a2, y2) = f a1 x1
--       etc. 
trace :: (a -> b -> (a, c)) -> a -> [b] -> [c]
trace _ _  []      = []
trace f a0 (x0:xs) = let (a1, y1) = f a0 x0
                     in y1 : trace f a1 xs

-- Cut a list into sublists of length n

splits :: Int -> [a] -> [[a]]
splits _ [] = []
splits n xs = let (prefix, suffix) = splitAt n xs in
              prefix : splits n suffix
              
