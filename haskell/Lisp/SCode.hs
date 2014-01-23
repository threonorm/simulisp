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
           deriving (Show)

data SData = SPtr SCons | SNum Int | SLocal Int Int | SGlobal String
           deriving (Show)

type SProgram = [(String, SWord)]

svoid :: SData
svoid = SNum 0

snil :: SWord
snil = SWord TNil svoid

