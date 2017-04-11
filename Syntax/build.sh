#!/bin/bash
set -e
set -u

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/..

bnfc -p Syntax Syntax/Grammar.cf
cp Syntax/fixErrM.hs Syntax/ErrM.hs
happy -gca Syntax/ParGrammar.y --info=Syntax/Grammar.info
alex -g Syntax/LexGrammar.x
ghc --make Syntax/TestGrammar.hs -o Syntax/TestGrammar
