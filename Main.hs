{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-}

module Main where
import Instances (main_Instances)
import Unification (main_Unification)
import Typecheck (main_Typecheck)

main = do
  main_Instances
  putStrLn "~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~"
  main_Unification
  putStrLn "~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~"
  main_Typecheck
