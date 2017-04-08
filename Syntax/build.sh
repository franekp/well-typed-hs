#!/bin/bash
set -e
set -u

bnfc -m Grammar.cf
cp fixErrM.hs ErrM.hs
make
happy -gca ParGrammar.y --info=Grammar.info
