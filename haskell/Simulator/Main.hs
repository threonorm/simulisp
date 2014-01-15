{-# LANGUAGE PatternGuards #-}

module Simulator.Main (main) where

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

import Netlist.AST
import Netlist.Parser
import Simulator.Scheduler
import Simulator.Simulator
import Simulator.InputParser

versionNumber :: String -- more flexible than int/float/whatever
versionNumber = "0.2"


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
          let launchSim inp = launchSimulation orderedNetlist (p_cycles p) inp (p_rom p) in
          case p_input p of
            Nothing -> launchSim Nothing 
            Just inputs -> case mapM (initialWireState orderedNetlist) inputs of
              Nothing -> failwith "Invalid inputs."
              Just initWS -> launchSim $ Just initWS

launchSimulation :: Program
                 -> Maybe Int
                 -> Maybe [WireState]
                 -> Maybe (Environment [Bool])
                 -> IO ()
launchSimulation netlist maybeCycles maybeInputs maybeROMs = do
  case (maybeCycles, maybeInputs) of
    (Just n, Just inp) | length inp < n ->
      putStrLn $ "Warning: not enough inputs to last for " ++ show n ++ " cycles."
    _ -> return ()
  let results = iteratedSimulation netlist maybeInputs maybeROMs
  mapM_ (putStrLn . formatOutputs) . maybe id take maybeCycles $ results

formatOutputs :: [(Ident, Value)] -> String
formatOutputs = intercalate "," . map (\(i,v) -> i ++ ":" ++ p v)
  where p (VBit b) = [c b]
        p (VBitArray bs) = map c bs
        c True = '1'
        c False = '0'

-- custom failwith to make sure exit code is 1
failwith :: String -> IO a
failwith e = hPutStrLn stderr e >> exitWith (ExitFailure 1)

data Option = Version
            | Help
            | InlineInput String
            | FileInput FilePath
            | FileROM FilePath
            | Cycles String
            deriving (Eq)

-- TODO: type alisases for ROM array and RAM map
data Params = P { p_filename :: FilePath
                , p_input    :: Maybe ([Environment Value])
                , p_rom      :: Maybe (Environment [Bool])
                , p_cycles   :: Maybe Int
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
                        rom <- getROM
                        cycles <- getCycles
                        return $ P { p_filename = filename
                                   , p_input    = (fmap.fmap) Map.fromList input
                                   , p_rom      = rom
                                   , p_cycles   = cycles }
      where getInput
              -- pattern guards to the rescue!
              | Just inputFileName <- findFileInput opts = do
                  eitherInput <- tryIOError $ parseFromFile inputParser inputFileName
                  case eitherInput of
                    Left _ -> failwith "Error opening input file."
                    Right (Left err) ->
                      failwith $ "Parse error on input file:\n" ++ show err
                    Right (Right input) -> return $ Just input
              | Just inputString <- findInlineInput opts =
                  case parse inlineInputParser "" inputString of
                    Left _ -> failwith "Input badly formatted."
                    Right input -> return $ Just [input]
              | otherwise = return Nothing
                            
            getROM
              | Just romFileName <- findFileROM opts = do
                  eitherROM <- tryIOError $ parseFromFile romParser romFileName
                  case eitherROM of
                    Left _ -> failwith "Error opening ROM file."
                    Right (Left err) ->
                      failwith $ "Parse error on ROM file:\n" ++ show err
                    Right (Right input) -> return . Just . Map.fromList $ input
              | otherwise = return Nothing

            getCycles
              | Just str <- findCycles opts = case readMaybe str of
                  Nothing -> failwith "Badly formatted option (num-cycles)."
                  Just n -> return (Just n)
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
                    "File containing list of inputs, every line is a new step of simulation."
                  , Option [] ["fROM"] (ReqArg FileROM "FILENAME")
                    "File containing the contents of the ROM."
                  , Option ['n'] ["num-cycles"] (ReqArg Cycles "N")
                    "Forces the simulation to stop after N cycles."
                  ]
        usage = "Usage: simulator --input=var1:(0|1)*,var2:(0|1)* FILE"
        helpMsg = usageInfo usage options
        versionMsg = "Simulisp version " ++ versionNumber ++ "."

        -- TODO: find a way to factor this pattern
        -- (Template Haskell?)
        findInlineInput = findExtract $ \x -> case x of
          InlineInput y -> Just y
          _             -> Nothing
        findFileInput = findExtract $ \x -> case x of
          FileInput y -> Just y
          _           -> Nothing
        findFileROM = findExtract $ \x -> case x of
          FileROM y -> Just y
          _           -> Nothing
        findCycles = findExtract $ \x -> case x of
          Cycles y -> Just y
          _        -> Nothing


-- This functions exists in GHC >= 7.6
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
        
findExtract :: (a -> Maybe b) -> [a] -> Maybe b
findExtract f xs = case f `mapMaybe` xs of
  []    -> Nothing
  (y:_) -> Just y
  
