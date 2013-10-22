module Main where

import Text.Parsec.String (parseFromFile)
import System.Environment

import NetlistParser
import NetlistPrint

main :: IO ()
main = do
  (filename:_) <- getArgs
  result <- parseFromFile netlistParser (filename ++ ".nl")
  case result of
    Left err -> putStrLn "error: " >> print err
    Right prog -> printProgToFile prog (filename ++ "-sorted.nl")

