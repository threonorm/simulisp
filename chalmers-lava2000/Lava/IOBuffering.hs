module Lava.IOBuffering where

import System.IO
  ( hSetBuffering
  , stdout
  , BufferMode(..)
  )

noBuffering :: IO ()
noBuffering = hSetBuffering stdout NoBuffering
