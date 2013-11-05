Simulisp 0.01
=============

Some tools to simulate digital circuits written in Netlist.

V0.01 contains : 
- A parser for netlists
- A scheduler (using topological sorting)
- A basic netlist interpreter

TODO:
- Memory
- Compilation of Netlist and Hot Code Swapping


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

To simulate juste one step:
`./Main --input=var1:(val1)+,...,vari:(vali)+ <netlist file name>`

To simulate n steps by feeding n inputs to the program through a file:
`./Main --finput=<input file name> <netlist file name>`

More information:
`./Main --help`

------------
Structure of input files
------------

    var ::= string                                                        
    val ::= binary number                                                 
                                                                          
    assign ::= var ':' number                                             
                                                                          
    line ::= assign                                                     
           | assign ',' line                                            
                                                                          
    file ::= line                                                        
           | line '\n' file                                              
                                                                          
Semantics :                                                     
- each line describes the inputs for one step of simulation           
- a line is a sequence of assignments of the form `variable:value`

WARNING : All the required inputs of your Netlists must be described in
your input file.

----------------------

The ultimate goal is to simulate a Lisp machine.
