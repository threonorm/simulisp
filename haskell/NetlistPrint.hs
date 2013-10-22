module NetlistPrint where

import Prelude hiding (mapM_)  
import Data.List
import Data.Char
import qualified Data.Map as Map

import NetlistAST
    
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
        f (Earg arg) =  formatArg arg
        f (Ereg ident) = "REG " ++ ident  
        f (Enot arg) = "NOT " ++ formatArg arg
        f (Ebinop binop arg arg2) = (map toUpper . show $ binop) ++ " "
                                    ++ formatArg arg ++ " " ++ formatArg arg2   
        f (Emux arg1 arg2 arg3) = "MUX " ++ 
              ( intercalate " " . map formatArg $ [arg1,arg2,arg3] ) 
        f (Erom int2 int1 arg) = intercalate " " 
            ["ROM", show int2, show int1, formatArg arg] 
        f (Eram int1 int2 arg1 arg2 arg3 arg4) = intercalate " " $ 
            ["RAM", show int2, show int1]++(map formatArg [arg1,arg2,arg3,arg4])  
        f (Econcat arg1 arg2) = "CONCAT "++ formatArg arg1 ++ formatArg arg2  
        f (Eslice int1 int2 arg) = "SLICE "++ show int1 ++ 
                                  show int2++ formatArg arg
        f (Eselect int arg) = "SELECT " ++ show int ++ formatArg arg
        

printProgToFile :: Program -> String -> IO ()
printProgToFile prog filename = writeFile filename $ printProg prog



