module Lisp.AsmScode2 where

import qualified Data.Map as Map

import Lisp.SCode
import Processor.Parameters
import Util.BinUtils

data FData = FPtr Int | FNum Int | FLocal Int Int | FGlobal String
           deriving (Show)
                    
type FWord = (Tag, FData)

type FCons = (FWord, FWord)

asmCons :: Int -> SCons -> ([FCons], Int)
asmCons thisAddr (scar, scdr) =
  let nextAddr = thisAddr + 1
      (fcar, fcaralloc, next2) = asmWord nextAddr scar
      (fcdr, fcdralloc, next3) = asmWord next2 scdr in
  ( ( (fcar, fcdr) : fcaralloc ++ fcdralloc ),
    next3)

asmWord :: Int -> SWord -> (FWord, [FCons], Int)
asmWord freeAddr (SWord tag (SPtr scons)) =
  let (fconses, nextAddr) = asmCons freeAddr scons in
  ((tag, FPtr freeAddr), fconses, nextAddr)
asmWord freeAddr (SWord tag x) = ((tag, f x), [], freeAddr)
  where f (SNum n)     = FNum n
        f (SLocal i j) = FLocal i j
        f (SGlobal s)  = FGlobal s


asmGlobalVar :: Int -> SWord -> ([FCons], Int)
asmGlobalVar i w = asmCons i (w, snil)

asmProgram :: SProgram -> [FCons] -- with property: no FGlobal in output
asmProgram sprog = map eliminateGlobals fconses
  where go _ [] = ([], [])
        go thisAddr ((ident, sword):rest) =
          let (progPrefix, nextAddr) = asmGlobalVar thisAddr sword
              (progSuffix, globalAddrsSuffix) = go nextAddr rest in
          (progPrefix ++ progSuffix, (ident,thisAddr):globalAddrsSuffix)
        (fconses, globalsAssocList) = go 0 sprog
        globalsMap = Map.fromList globalsAssocList
        eliminateGlobals (car, cdr) = (eg car, eg cdr)
        eg (tag, FGlobal s) = (tag, FPtr $ globalsMap Map.! s)
        eg (tag, x) = (tag, x)

fwordToBinary :: FWord -> [Bool]
fwordToBinary (tag, fdata) = tagBin tag ++ pdata fdata
  where pdata (FPtr n) = decToBools dataS n
        pdata (FNum n) = decToBools dataS n
        pdata (FLocal i j) = decToBools lowerS j ++ decToBools upperS i

serializeSCode :: SProgram -> String
serializeSCode = boolsToString . concatMap f . asmProgram
  where boolsToString = map (\b -> if b then '1' else '0')
        f (car, cdr) = fwordToBinary car ++ fwordToBinary cdr



