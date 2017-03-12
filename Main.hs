{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Main where
import Base.TypesImpl (main_TypesImpl)
import Semantics.Unification (main_Unification)
import Semantics.Typecheck (main_Typecheck)

main = do
  main_TypesImpl
  putStrLn "~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~"
  main_Unification
  putStrLn "~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~"
  main_Typecheck
