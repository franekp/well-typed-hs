{-# LANGUAGE CPP #-}
#include "settings.hs"

module Main where
import Base.Examples (testTypesImpl)
import Semantics.Unify (testUnify)
import Semantics.Typecheck (testTypecheck)

testMain = all id [testTypesImpl, testUnify, testTypecheck]

main = if testMain then putStrLn "OK" else putStrLn "FAIL"
