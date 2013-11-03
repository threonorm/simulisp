Simulisp 0.01
--------------
Some tools to simulate digital circuits written in Netlist.

V0.01 contains : 
-A Lexer/Parser of Netlist
-A topological sort
-A basic interpreter of Netlist

TODO:
-Memory
-Compilation of Netlist and Hot Code Swapping


-------------
INSTALLATION
-------------

Requirements :
- Haskell Platform >= 2013.2.0.0

Building : 

cd ./simulisp/haskell
ghc --make Main

------------
USAGE
------------

./Main --input=var1:(val1)+,...,vari:(vali)+ fileNetList


More informations with :
./Main --help


-------------
The ultimate goal is to simulate a Lisp machine.
