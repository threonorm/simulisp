module Lisp.AsmScode where

import Lisp.SCode
import Processor.Parameters
import Util.BinUtils
import Data.List
import Data.Function
import qualified Debug.Trace as T

--unfoldStep :: State Int (Queue (SWord,Int,Maybe Int)) -> State Int (Maybe ((SWord,Int,Maybe Int),Queue (SWord,Int,Maybe Int)))  
--unfoldStep monQueue = do queue <- monQueue
--                         if isNothing(monDeQueue) 
--                            then return $ (Nothing,emptyQueue)
--                            else let Just ((word,source,dest),newQueue) = monDeQueue
--                                     newPtr = catMaybes $ [ptr1W word, ptr2W word] in
--                                 if newPtr == [] then
--                                      return $ (Just (a,source,Nothing),newQueue)
--                                      else do fresh <- get
--                                              put $ fresh + 2
--                                              let newPtr = catMaybes.map globM $  [(ptr1W word,fresh,Nothing),(ptr2W word,fresh+1,Nothing)] 
--                                                  finalQueue = addQueue newQueue . zip3 newPtr [fresh,fresh+1]  $ repeat Nothing    
--                                              return $ (Just (a,source,fresh),finalQueue)   
--                      where monDeQueue = deQueue queue
--                            ptr1W = undefined
--                            ptr2W = undefined
--                            addQueue q [] = q
--                            addQueue q (h::t) = addQueue (addToQueue q h) t
--                            globM (Just a, b, c) = Just (a, b, c)
--                            globM (Nothing , b, c) = Nothing
                      
assemble :: SProgram -> String
assemble prog = 
   concat . concat . interToString . linearizeProg prog $ 0   


linearizeProg :: SProgram -> Int -> [(String,[(SWord,Int)],Int)]
linearizeProg [] n = []
linearizeProg ((stg,sword):q) n = 
        T.trace (show n ++ " | " ++ show list ++"\n") $ (stg, list, n):(linearizeProg q (pos+1) )   
  where (list,pos) = unfoldTree sword n


interToString :: [(String,[(SWord,Int)],Int)] -> [[String]]
interToString list = 
  map functionToString $ list 
  where functionToString (a,b,c) = 
          map swordToString . hack .sortBy (compare `on` snd) $ b 
        swordToString (SWord tag d,b) =
          tagToString tag ++ 
          case d of
            SPtr cons  -> (decToBin (dataS-1) $ b+1)++"0" --Code in ROM
            SNum i     -> decToBin dataS $ i
            SLocal i j -> (decToBin lowerS $ j) ++ (decToBin upperS $ i )   
            SGlobal s  -> (decToBin (dataS-1)  $ posGlobal s list) ++ "0" --idem
        posGlobal s ((a,b,c):q)= if a == s then c else posGlobal s q 
        posGlobal s [] = undefined  --We suppose that the global exist
        hack (t:q) = t:((snil, 0):q)


unfoldTree :: SWord -> Int -> ([(SWord,Int)],Int)
unfoldTree tree pos =
  case ptr tree of
    Nothing -> ([(tree,pos)],pos)
    Just (w1,w2) -> let (t1,intPos) = unfoldTree w1 $ pos + 1
                        (t2,newPos) = unfoldTree w2 $ intPos in
                    ((tree,pos):(t1++t2),newPos)
  where ptr (SWord tag d)=
          case d of
            SPtr (a,b) -> Just (a,b)
            _          -> Nothing


                               
tagToString :: Tag -> String
tagToString tag = map boolToChar . tagBin $ tag
    where boolToChar True = '1'
          boolToChar False = '0'

