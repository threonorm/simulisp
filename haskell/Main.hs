module Main (main) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import System.IO.Error
import Text.Parsec.String (parseFromFile)

import NetlistAST
import NetlistParser
import Scheduler
import Simulator


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
        Nothing -> failwith "The netlist contains a combinational cycle"
        Just orderedNetlist ->
          case simulateOneCycle orderedNetlist (p_input p) of
            Nothing -> failwith "Invalid inputs"
            Just x -> putStrLn $ formatOutputs x

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
            | Input (Maybe (Environment Value))
            deriving (Eq)
data Params = P { p_filename :: FilePath
                , p_input :: Maybe (Environment Value)
                }
              deriving (Eq)

getParams :: IO Params
getParams = do
  o <- getOpt Permute options <$> getArgs
  case o of
    (opts, [filename], [])
      | Help `elem` opts -> putStrLn helpMsg >> exitSuccess
      | Version `elem` opts -> putStrLn versionMsg >> exitSuccess
      | otherwise -> return $ P { p_filename = filename
                                , p_input = findInput opts }
                     -- TODO: handle unrecognized arguments
    (_, _, []) -> failwith helpMsg
    (_, _, errors) -> failwith $ concat errors ++ helpMsg
  where options = [ Option ['v'] ["version"] (NoArg Version)
                    "Print simulator version number."
                  , Option ['h'] ["help"] (NoArg Help)
                    "Print this help message."
                  , Option [] ["input"] (OptArg (Input . fmap parseInput) "INPUT")
                    "List of inputs to provide to the circuit."
                  ]
        usage = "Usage: simulateur --input=var1:(0|1)*,var2:(0|1)* FILE"
        helpMsg = usageInfo usage options
        versionMsg = "Simulisp version " ++ versionNumber ++ "."
        parseInput = Map.fromList . map q . unintersperse ','
          -- TODO: signal badly formatted input instead of failing miserably
          --       at some random time
          where q s = let (ident, _:s') = break (== ':') s
                      in (ident, p s')
                p [c] = VBit $ c /= '0'
                p cs = VBitArray $ map (/= '0') cs
        findInput = head . catMaybes . map f
          where f (Input x) = Just x
                f _ = Nothing
        
