{-# LANGUAGE QuasiQuotes #-}

module Lisp.ClockProgram where

import Lisp.MultilineQuote
import Lisp.MiniLispParser
import Lisp.MiniLispCompiler
import Lisp.AsmScode
import Lisp.AsmScode2

import Text.Parsec

clockProgram = [multilineQuote|
(defun main ()
  (clock 0 0))

(defun clock (sec min)
  (print-minute min)
  (print-second sec)
  (let ((new-sec (+1 sec)))
    (if (>=60? new-sec)
        (synchronize (clock 0 (+1 min)))
        (synchronize (clock new-sec min)))))
|]

(Right lispProg) = parse miniLispParser "" clockProgram

universalAnswer = [multilineQuote|
(defun main ()
  (+1 41))
|] 

(Right lisp1Prog) = parse miniLispParser "" universalAnswer
scodeAnswer = compileProgram lisp1Prog 


scode = compileProgram lispProg                

main =
  let Just compiled = scode in 
  putStrLn $"rom_code:"++serializeSCode compiled


