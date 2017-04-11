#!/bin/bash
set -e
set -u

if [ $(hostname) == "students" ] ; then
  export PATH = /home/students/inf/PUBLIC/MRJP/ghc-7.10.2/bin/:$PATH
fi
which ghc
ghc -package ghc -XCPP -DGHC_LIBDIR="\"$(ghc --print-libdir)\"" --make Main.hs
