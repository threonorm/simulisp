#!/bin/sh

cd haskell
cabal configure 
cabal build 
`./dist/build/generate-processor/generate-processor`
`./generateRom.sh rom`
cp rom ../.
cp processor.net ../.
cp dist/build/simulator/simulator ../.
echo "\nUse \"./simulator --fROM=rom --clock-mode processor.net\" to run the clock!"
    
