{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-} {-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Main where
import Base.Examples (testTypesImpl)
import Semantics.Unify (testUnify)
import Semantics.Typecheck (testTypecheck)

testMain = all id [testTypesImpl, testUnify, testTypecheck]

main = if testMain then putStrLn "OK" else putStrLn "FAIL"
