{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Main where
import Base.TypesImpl (testTypesImpl)
import Semantics.Unification (testUnification)
import Semantics.Typecheck (testTypecheck)

testMain = all id [testTypesImpl, testUnification, testTypecheck]

main = if testMain then putStrLn "OK" else putStrLn "FAIL"
