module Lisp.SCode where

import Lisp.Primitives

data Tag = TNil
         | TLocal
         | TGlobal
         | TClosure
         | TCond
         | TList
         | TNum
         | TProc
         | TFirst
         | TNext
         | TLast
         | TApplyOne
         | TLet
         | TSequence
         | TSync
         | TPrim Primitive
         deriving (Eq, Show)
                  
