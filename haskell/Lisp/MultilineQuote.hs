module Lisp.MultilineQuote where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
multilineQuote = QuasiQuoter { quoteExp = stringE }

