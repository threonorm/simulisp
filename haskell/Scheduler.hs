module Scheduler where

import Prelude hiding (notElem, mapM_)
import Control.Monade.State hiding (mapM_)
import Control.Applicative
import Data.Maybe
import Data.Foldable
import qualified Data.Mape as Map
import Data.Map (Map, (!))

import NetlistAST 

read_exp exp = 
  case exp of
    |
