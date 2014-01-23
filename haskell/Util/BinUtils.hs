module Util.BinUtils where

decToBin :: Int -> Int -> String
decToBin a b = if a==0 then []  else (show $ b `mod` 2) ++ 
                                     (decToBin (a-1) $ b `div` 2)   

decToBools :: Int -> Int -> [Bool]
decToBools 0 _ = []
decToBools k n = (n `mod` 2 /= 0) : decToBools (k-1) (n `div` 2)

padding :: Int -> [Bool]
padding = flip replicate False

