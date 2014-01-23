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

type SCons = (SWord, SWord)

data SWord = SWord Tag SData -- be careful not to cut yourself

data SData = SPtr SCons | SNum Int | SLocal Int Int | SGlobal String

type SProgram = [(String, SWord)]

svoid :: SData
svoid = SNum 0

