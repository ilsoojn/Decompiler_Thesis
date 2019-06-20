# Decompiler Framework

LLVM-IR Based Decompiler's Middle-end Framework with 'repzret/dagger' binary translator.

The code is built for master thesis and currenlty under development.

Framework does not currently support 'Function Call/Return' and 'Pointers' (+ 'char' typed varaibles) analysis.

### Prerequisites

The code is written in Haskell and uses Linux command lines, LLVM-IR, and an open-source 'Dagger' framework.

There might be ghc-packages required to additinally install.


More information on how to install and use [Haskell](https://www.haskell.org/downloads/), [LLVM](http://llvm.org/docs/GettingStarted.html), and [Dagger](https://github.com/repzret/dagger) pages.

### Command Line Usage
$ runhaskell ir.hs <binaryFile> <option>

or

$ ghc -o <outputFileName> ir.hs

$ ./<outputFileName> <binaryFile> <option>


Options:

* preprocess - output a LLVM-IR code after applying 'Dagger'

* idiom - outut a IR code after the idiom detection and conversion

* propagation - outut a IR code after variable propagation

* variable_name - outut a IR code after renaming SSA formated variables

* precision - outut a IR code after converting the float-point percision

* elimination - outut a IR code after the elimination stage

* HLL - outut a high-level language (HLL) formatted code after the HLL generation phase


## Built with

'objdump' and 'disassemble' Linux command lines

[Dagger](https://github.com/repzret/dagger) - Binary transformation framework

