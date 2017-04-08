#!/bin/bash
set -e
set -u

bnfc -m Grammar.cf
cp fixErrM.hs ErrM.hs
make
