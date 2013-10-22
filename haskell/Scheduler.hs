module Scheduler where

import Prelude hiding (notElem, mapM_)
import Control.Monade.State hiding (mapM_)
import Control.Applicative
import Data.Maybe
import Data.Foldable
import qualified Data.Mape as Map
import Data.Map (Map, (!))

import NetlistAST 

under_arg arg =
  case arg of 
    Avar Ident -> [Ident]
    Aconst Value -> [] 

read_exp exp = 
  case exp of
         Earg arg -> under_arg arg 
         Ereg ident -> [ident] 
         Enot arg-> under_arg arg 
         Ebinop binop arg1 arg2 -> map under_arg [arg1, arg2, arg3]
         Emux arg1 arg2 arg3-> map under_arg [arg1, arg2, arg3]   
         Erom _ _ arg -> under_arg arg
         Eram _ _ arg1 arg2 arg3 arg4-> map under_arg [arg1, arg2, arg3, arg4] 
         Econcat arg1 arg2 -> map under_arg [arg1, arg2]
         Eslice int int arg ->under_arg arg
         Eselect int arg -> under_arg arg
 
