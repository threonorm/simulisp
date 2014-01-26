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
linearizeProg ((stg,sword):q) n = (stg, list, n):(linearizeProg q (pos) )   
  where (list,pos,blabla) = unfoldTree sword n True 

interToString :: [(String,[(SWord,Int,Maybe Int)],Int)] -> [[String]]
interToString list = map functionToString $ list 
  where functionToString (a,b,c) = 
          map swordToString . hack .sortBy (compare `on` (\(x,y,z)->y)) $ b 
        swordToString (SWord tag d,b,Just l) =
          tagToString tag ++ 
          case d of
            SPtr cons  -> (decToBin (dataS-1) $ l)++"0" --Code in ROM
            SNum i     ->  decToBin dataS $ i           --3 hack-lines  
            SLocal i j -> (decToBin lowerS $ j) ++ (decToBin upperS $ i )   
            SGlobal s  -> (decToBin (dataS-1)  $ posGlobal s list) ++ "0" 
        swordToString (SWord tag d,b,Nothing) =
          tagToString tag ++ 
          case d of
            SNum i     ->  decToBin dataS $ i
            SLocal i j -> (decToBin lowerS $ j) ++ (decToBin upperS $ i )   
            SGlobal s  -> (decToBin (dataS-1)  $ posGlobal s list) ++ "0" --idem
        posGlobal s ((a,b,c):q) = if a == s then c else posGlobal s q 
        posGlobal s [] = undefined                     --We suppose that the global exist
        hack (t:q) = t:(snil,0,Nothing):q
          
unfoldTree :: SWord -> Int -> Bool -> ([(SWord,Int,Maybe Int)],Int,Bool)
unfoldTree tree pos isCar =
  case ptr tree of
    Nothing -> if isCar then ([(tree,pos,Nothing)],pos,True) 
                        else ([(tree,pos,Nothing)],pos+1,False)
    Just (w1,w2) -> let (t1,intPos,incomplete1)      = unfoldTree w1 (pos+1)  True   
                        ((h2:t2),newPos,incomplete2) = unfoldTree w2 (intPos) False  in
                        if incomplete1 then ((tree,pos,Just (pos+1)) :
                            (t1 ++ (h2 : t2)),newPos,incomplete2)
                          else ((tree,pos,Just (pos+1)):(t1 ++ 
                              ((\(x,y,z)->x) h2,pos+1,Just (intPos)) :
                              (map f t2)),newPos-1,incomplete2)
  where ptr (SWord tag d) = case d of
                              SPtr (a,b) -> Just (a,b)
                              _          -> Nothing
        f (a,b,Just c) = (a,b-1,Just $ c-1)
        f (a,b,Nothing)= (a,b-1,Nothing)
                               
tagToString :: Tag -> String
tagToString tag = map boolToChar . tagBin $ tag
    where boolToChar True = '1'
          boolToChar False = '0'
