module Main where

import Text.Parsec.String (parseFromFile)
import System.Environment
import Data.Maybe

import NetlistParser
import NetlistPrint
import Scheduler 

main :: IO ()
main = do
  (filename:_) <- getArgs
  result <- parseFromFile netlistParser (filename ++ ".nl")
  case result of
    Left err -> putStrLn "error: " >> print err
    Right prog -> case schedule prog of
        Just a -> printProgToFile a (filename ++ "-sorted.nl")
        Nothing -> putStrLn "Topo"
