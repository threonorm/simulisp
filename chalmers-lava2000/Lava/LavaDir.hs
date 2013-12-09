module Lava.LavaDir where

import Paths_chalmers_lava2000

getLavaDir :: IO FilePath
getLavaDir = getDataDir

