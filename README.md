Li(sp)Monade (Simulisp) 0.2
============

A very basic processor simulated at the logic gate level, based on an
experimental Lisp machine architecture brought back from the dead, and
designed using copious amounts of Haskell.

This is actually a school project for a hardware architecture course
at Ecole Normale Supérieure.

Contents:
* Some tools to simulate synchronous digital circuits written in a
simple netlist language
* An emulator for the processor (in imperative-style OCaml)
* A high-level hardware description language embedded in Haskell,
  inspired by Lava
* A toolset to write the microcode
* A Mini-Lisp compiler and an assembler



Installation
------------

### Requirements

- Haskell Platform >= 2013.2.0.0, including the GHC compiler >= 7.6
  and the Parsec and Vector packages
- (For the simulator's clock-mode) hsSDL >= 0.6.5
- (For the emulator only) OCaml >= 4.00

### Building

The easy way to build the complete project is use 

    ./build.sh

Be patient! Compilation can take a few minutes. If you are in a hurry
you can delete the -O2 options in the file ./haskell/simulisp.cabal

If you prefer a more hands-on way :

We use Cabal as our build system.

    cd ./simulisp/haskell
    cabal configure
    cabal build

The binaries generated are:
- the simulator in `dist/build/simulator/simulator`
- a program to generate the netlist of the processor
  in `dist/build/generate-processor/generate-processor`
- a program to generate the microprogram of the processor
  in `dist/build/generate-microprogram/generate-microprogram`
- a program to generate the binary file of the clock
  in `dist/build/clock-program/clock-program`

You can build the rom file in the good form using the script :
      
      ./generateRom.sh

It uses the last two binaries which we just presented.

Simulator usage
---------------

To simulate juste one step:
`./simulator --input=var1:(val1)+,...,vari:(vali)+ <netlist file name>`

To simulate n steps by feeding n inputs to the program through a file:
`./simulator --finput=<input file name> <netlist file name>`

More information:
`./simulator --help`

### Structure of input files

    var ::= string                                                        
    val ::= binary number                                                 
                                                                          
    assign ::= var ':' number                                             
                                                                          
    line ::= assign                                                     
           | assign ',' line                                            
                                                                          
    file ::= line                                                        
           | line '\n' file                                              
                                                                          
Semantics:                                                     
- the nth line describes the inputs for the nth simulation step
- a line is a comma-separated sequence of assignments of the form
  `variable:value`

WARNING: All the required inputs of your Netlists must be described in
your input file.

### Structure of ROM files

A succession of lines, each of which is of the type

    <ID of the ROM chip in the netlist>:<contents in binary>

