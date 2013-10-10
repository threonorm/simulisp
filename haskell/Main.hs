module Main where

import Text.Parsec.String (parseFromFile)

import NetlistParser

main :: IO ()
main = do
  result <- parseFromFile netlistParser "fulladder.nl"
  case result of
    Left err -> putStrLn "error: " >> print err
    Right ast -> print ast
