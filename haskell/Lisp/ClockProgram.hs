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
  (count-hours 18))

(defun count-hours (hr)
  (print-hour hr)
  (count-minutes 0)
  (let ((new-hr (+1 hr)))
    (if (>=24? new-hr)
        (synchronize (count-hours 0))
        (synchronize (count-hours new-hr)))))

(defun count-seconds (sec)
  (print-second sec)
  (let ((new-sec (+1 sec)))
    (if (>=60? new-sec)
        ()
        (synchronize (count-seconds new-sec)))))

(defun count-minutes (min)
  (print-minute min)
  (count-seconds 0)
  (let ((new-min (+1 min)))
    (if (>=60? new-min)
        ()
        (synchronize (count-minutes new-min)))))

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
  putStrLn $"rom_code:"++assemble compiled


