module Netlist.Print where

import Prelude hiding (mapM_)  
import Data.List
import Data.Char
import qualified Data.Map as Map

import Netlist.AST
    
printProg :: Program -> String
printProg prog =
  "INPUT " ++
  intercalate ", " (p_inputs prog) ++ 
  "\nOUTPUT " ++
  intercalate ", " (p_outputs prog) ++
  "\nVAR " ++
  (intercalate ", " . map formatVar . Map.toList $ p_vars prog ) ++
  "\nIN\n"++
  (intercalate "\n" . map formatEq $ p_eqs prog) 
  
  where formatVar (ident,TBit) =  ident
        formatVar (ident,TBitArray n) = ident ++ " : " ++ show n
        formatEq (ident,expr) =
          ident++ " = " ++  f expr
        formatArg (Avar ident)= ident
        formatArg (Aconst (VBit a)) = [bitToChar a]
        formatArg (Aconst (VBitArray t)) = map bitToChar t
        bitToChar True = '1'
        bitToChar False = '0'
        f (Earg arg) = formatArg arg
        f (Ereg ident) = p ["REG", ident]
        f (Enot arg) = p ["NOT", formatArg arg]
        f (Ebinop binop arg1 arg2) =
          p [(map toUpper . show $ binop), formatArg arg1, formatArg arg2]
        f (Emux arg1 arg2 arg3) =
          p $ "MUX" : map formatArg [arg1,arg2,arg3]
        f (Erom int1 int2 arg) =
          p ["ROM", show int1, show int2, formatArg arg] 
        f (Eram int1 int2 arg1 arg2 arg3 arg4) =
          p $ ["RAM", show int1, show int2]
              ++ (map formatArg [arg1,arg2,arg3,arg4])  
        f (Econcat arg1 arg2) =
          p ["CONCAT", formatArg arg1, formatArg arg2]
        f (Eslice int1 int2 arg) =
          p ["SLICE", show int1, show int2, formatArg arg]
        f (Eselect int arg) = p ["SELECT", show int, formatArg arg]
        p = intercalate " "
        

printProgToFile :: Program -> FilePath -> IO ()
printProgToFile prog filename = writeFile filename $ printProg prog



