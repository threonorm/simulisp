module Util.BinUtils where

decToBin :: Int -> Int -> String
decToBin a b = if a==0 then []  else (show $ b `mod` 2) ++ 
                                     (decToBin (a-1) $ b `div` 2)   
