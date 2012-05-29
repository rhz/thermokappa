ThermoKappa
===========

Library for compiling thermodynamical Kappa into Kappa.

Well, at least that's the goal. For now, what I have is a small program to compute *minimal glueings*.

## MinimalGlueings

Compile: `ghc --make -O MinimalGlueings`

To compile it, you will need GHC (Glasgow Haskell Compiler) and the [`vector`](http://hackage.haskell.org/package/vector), [`parsec`](http://hackage.haskell.org/package/parsec) and [`indents`](http://hackage.haskell.org/package/indents) libraries.

Usage: `./MinimalGlueings inputfile.ka`

The input file must contain two Kappa expressions separated by a semicolon.

The program will generate one DOT file with all minimal glueings in textual representation plus one DOT file for each minimal glueing detailing agent mappings in a graphical way.
