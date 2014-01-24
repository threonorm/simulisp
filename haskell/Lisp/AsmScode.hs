module Lisp.AsmScode where

import Lisp.SCode
import Processor.Parameters
import Util.BinUtils
import Data.List
import Data.Function
import qualified Debug.Trace as T

assemble :: SProgram -> String
assemble prog = 
   concat . concat . interToString . linearizeProg prog $ 0   

linearizeProg :: SProgram -> Int -> [(String,[(SWord,Int,Maybe Int)],Int)]
linearizeProg [] n = []
linearizeProg ((stg,sword):q) n = 
         T.trace (show list) $ (stg, list, n):(linearizeProg q (pos+1) )   
  where (list,pos) = unfoldTree sword n True

interToString :: [(String,[(SWord,Int,Maybe Int)],Int)] -> [[String]]
interToString list = 
  map functionToString $ list 
  where functionToString (a,b,c) = 
          map swordToString . hack .sortBy (compare `on` (\(x,y,z)->y)) $ b 
        swordToString (SWord tag d,b,Just l) =
          tagToString tag ++ 
          case d of
            SPtr cons  -> (decToBin (dataS-1) $ l)++"0" --Code in ROM
        swordToString (SWord tag d,b,Nothing) =
          tagToString tag ++ 
          case d of
            SNum i     ->  decToBin dataS $ i
            SLocal i j -> (decToBin lowerS $ j) ++ (decToBin upperS $ i )   
            SGlobal s  -> (decToBin (dataS-1)  $ posGlobal s list) ++ "0" --idem
        posGlobal s ((a,b,c):q)= if a == s then c else posGlobal s q 
        posGlobal s [] = undefined  --We suppose that the global exist
        hack (t:q) = t:((snil, 0, Nothing):q)

unfoldTree :: SWord -> Int -> Bool -> ([(SWord,Int,Maybe Int)],Int)
unfoldTree tree pos isCar =
  case ptr tree of
    Nothing -> if isCar then ([(tree,pos,Nothing)],pos) 
                        else ([(tree,pos,Nothing)],pos+1)
    Just (w1,w2) -> let (t1,intPos) = unfoldTree w1 (pos+1)  $ True 
                        (t2,newPos) = unfoldTree w2 (pos+1)  $ False in
                        if isCar 
                          then ((tree,pos,Just (pos+1)):(t1++t2),newPos)
                          else ((tree,pos,Just (intPos)):(t1++t2),newPos)
  where ptr (SWord tag d)=
          case d of
            SPtr (a,b) -> Just (a,b)
            _          -> Nothing
                               
tagToString :: Tag -> String
tagToString tag = map boolToChar . tagBin $ tag
    where boolToChar True = '1'
          boolToChar False = '0'
