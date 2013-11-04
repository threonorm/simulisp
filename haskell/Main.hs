module Main (main) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import System.IO.Error
import Text.Parsec (parse)
import Text.Parsec.String (parseFromFile)

import NetlistAST
import NetlistParser
import Scheduler
import Simulator
import InputParser

versionNumber :: String -- more flexible than int/float/whatever
versionNumber = "0.1"


main :: IO ()
main = do
  p <- getParams
  netlist'' <- tryIOError $ parseFromFile netlistParser (p_filename p)
  -- ugly error handling, TODO: fix that with monads and combinators ?
  case netlist'' of
    Left _ -> failwith "Could not open file.\n"
    Right netlist' -> case netlist' of
      Left err -> failwith $ "Parse error:\n" ++ show err
      Right netlist -> case schedule netlist of
        Nothing -> failwith "The netlist contains a combinational cycle."
        Just orderedNetlist ->
          let launchSim = launchSimulation orderedNetlist in
          case p_input p of
            Nothing -> launchSim Nothing
            Just inputs -> case mapM (initialWireState orderedNetlist) inputs of
              Nothing -> failwith "Invalid inputs."
              Just initWS -> launchSim $ Just initWS

launchSimulation :: Program -> Maybe [WireState] -> IO ()
launchSimulation netlist maybeInputs =
  let results = iteratedSimulation netlist maybeInputs in
  mapM_ (putStrLn . formatOutputs) results

formatOutputs :: [(Ident, Value)] -> String
formatOutputs = intercalate "," . map (\(i,v) -> i ++ ":" ++ p v)
  where p (VBit b) = [c b]
        p (VBitArray bs) = map c bs
        c True = '1'
        c False = '0'

-- custom failwith to make sure exit code is 1
failwith :: String -> IO a
failwith e = hPutStrLn stderr e >> exitWith (ExitFailure 1)

-- for some reason this is not in the standard libraries
unintersperse :: (Eq a) => a -> [a] -> [[a]]
unintersperse x xs = let (y, rest) = break (== x) xs
                     in y : case rest of
                       []    -> []
                       (_:q) -> unintersperse x q

data Option = Version
            | Help
            | InlineInput String
            | FileInput String
            deriving (Eq)

data Params = P { p_filename :: FilePath
                , p_input :: Maybe ([Environment Value])
                }
              deriving (Eq)

getParams :: IO Params
getParams = do
  o <- getOpt Permute options <$> getArgs
  case o of
    (opts, [filename], [])
      | Help `elem` opts -> putStrLn helpMsg >> exitSuccess
      | Version `elem` opts -> putStrLn versionMsg >> exitSuccess
      | otherwise -> do input <- getInput
                        return $ P { p_filename = filename
                                   , p_input = (fmap.fmap) Map.fromList input }
      where getInput
              -- pattern guards to the rescue!
              | Just inputFileName <- findFileInput opts = do
                  eitherInput <- tryIOError $ parseFromFile inputParser inputFileName
                  case eitherInput of
                    Left _ -> failwith "Error opening file."
                    Right (Left err) ->
                      failwith $ "Parse error on input file:\n" ++ show err
                    Right (Right input) -> return $ Just input
              | Just inputString <- findInlineInput opts =
                  case parse inlineInputParser "" inputString of
                    Left _ -> failwith "Input badly formatted."
                    Right input -> return $ Just [input]
              | otherwise = return Nothing

    -- TODO: handle unrecognized arguments
    (_, _, []) -> failwith helpMsg
    (_, _, errors) -> failwith $ concat errors ++ helpMsg
    
  where options = [ Option ['v'] ["version"] (NoArg Version)
                    "Print simulator version number."
                  , Option ['h'] ["help"] (NoArg Help)
                    "Print this help message."
                  , Option [] ["input"] (ReqArg InlineInput "INPUT")
                    "List of inputs to provide to the circuit."
                  , Option [] ["finput"] (ReqArg FileInput "FILENAME") 
                    "File containing list of inputs, every line is a new step of simulation."]
        usage = "Usage: simulateur --input=var1:(0|1)*,var2:(0|1)* FILE"
        helpMsg = usageInfo usage options
        versionMsg = "Simulisp version " ++ versionNumber ++ "."

        findInlineInput = findExtract $ \x -> case x of
          InlineInput y -> Just y
          _             -> Nothing
        findFileInput = findExtract $ \x -> case x of
          FileInput y -> Just y
          _           -> Nothing
        
        
findExtract :: (a -> Maybe b) -> [a] -> Maybe b
findExtract f xs = case f `mapMaybe` xs of
  []    -> Nothing
  (y:_) -> Just y
  
