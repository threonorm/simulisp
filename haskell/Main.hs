module Main where

import NetlistAST
import NetlistParser

main = do
  result <- Text.Parsec.String.parseFromFile "fulladder.nl"
  case result of
    Left err -> putStrLn "error: " >> print err
    Right ast -> print ast
