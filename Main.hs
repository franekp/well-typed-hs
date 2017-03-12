{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Main where
import Base.TypesImpl (testTypesImpl)
import Semantics.Unify (testUnify)
import Semantics.Typecheck (testTypecheck)

testMain = all id [testTypesImpl, testUnify, testTypecheck]

main = if testMain then putStrLn "OK" else putStrLn "FAIL"
