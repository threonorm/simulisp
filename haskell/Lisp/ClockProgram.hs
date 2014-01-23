{-# LANGUAGE QuasiQuotes #-}

module Lisp.ClockProgram where

import Lisp.MultilineQuote
import Lisp.MiniLispParser
import Lisp.MiniLispCompiler
import Lisp.AsmScode

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



banane = [multilineQuote|
(defun main ()
  (+1 41))
|] 

(Right lisp1Prog) = parse miniLispParser "" banane
scode = compileProgram lispProg                
scode1 = compileProgram lisp1Prog 

