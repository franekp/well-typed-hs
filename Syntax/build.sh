#!/bin/bash
set -e
set -u

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/..

bnfc -p Syntax -m Syntax/Grammar.cf
cp Syntax/fixErrM.hs Syntax/ErrM.hs
make
happy -gca Syntax/ParGrammar.y --info=Syntax/Grammar.info
