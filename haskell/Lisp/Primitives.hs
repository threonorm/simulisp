module Lisp.Primitives where


data Primitive = PCar
               | PCdr 
               | PCons
               | PIncr
               | PDecr
               | PIsZero 
               | PIsgt60
               | PIsgt24
               | PPrintSec
               | PPrintMin
               | PPrintHour
               | PPrintDay
               | PPrintMonth
               | PPrintYear
               deriving (Eq, Show)
