{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-} {-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns #-}

module Base.UAst where

data UAst = AddUA
  | LiteralUA Int
  | AppUA UAst UAst
  | LambdaUA String UPolyType UAst
  | VarUA String
  | LetUA String UAst UAst

data UMonoType = IntUMT | ArrowUMT UMonoType UMonoType | VarUMT String
data UPolyType = ForallUPT String UPolyType | MonoUPT UMonoType
