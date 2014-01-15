{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Data.List

import Assembler


--A toy example


example = [ Label("test",0) ,
            Value ^= Expr,
            jmp "test",
            Label("Hum",22),
            Expr ^= Stack,
            condJump "test",
            fetchCar Value Expr      
          ]


main :: IO ()
main = case assemble example of 
            Nothing -> putStrLn "Fail"
            Just a -> putStrLn a  
