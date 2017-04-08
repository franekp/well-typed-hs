{-# LANGUAGE CPP #-}
#include "settings.hs"

module Main where
import Semantics.Tests (tests)

main = if tests then putStrLn "OK" else putStrLn "FAIL"
