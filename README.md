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

To simulate juste one step :
./Main --input=var1:(val1)+,...,vari:(vali)+ fileNetList


To simulate N step :
./Main --finput="filename" fileNetList

------------
Structure of input files
------------
var ::= string
val ::= binary number

assign ::= var ':' number

line ::=  |assign
          |assign ',' line 

file ::= |line
         |line '\n' file

The semantic is : 
  - one line describe the inputs for one step of simulation
  - a line is a sequence of assignments of the form "variable : value"

WARNING : All the required inputs of your Netlists must be described in
your input file.


----------------------
Some informations with :
./Main --help

-------------
The ultimate goal is to simulate a Lisp machine.
