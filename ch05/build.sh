#!/bin/sh

ghc -c SimpleJSON.hs

# no reason to include SimpleJSON.o as ghc will automatically pick it up
# if you do include it, ld will complain about duplicate symbols
ghc -o simple Main.hs
