{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, GADTs, RankNTypes, StandaloneDeriving, DeriveDataTypeable, ScopedTypeVariables,
FunctionalDependencies, OverlappingInstances, FlexibleInstances, FlexibleContexts, ExistentialQuantification, UndecidableInstances,
TypeFamilies #-} {-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns -Werror #-}

module Base.Examples where
import Base.Pervasives
import Base.UAst
import Base.Types
import Base.TypesImpl

polytype_examples :: [Poly Type]
polytype_examples = [
    ForallP 1 (ExistsPoly $
    ForallP 2 (ExistsPoly $
      MonoP $ Mono $ (anything :: Type (a -> (a -> b) -> b))
    :: forall b. A Type b => ExistsPoly Type b)
    :: forall a. A Type a => ExistsPoly Type a)
  ,
    ForallP 3 (ExistsPoly $
      MonoP $ Mono $ (anything :: Type ((a -> a) -> (a -> a) -> a -> a))
    :: forall a. A Type a => ExistsPoly Type a)
  ,
    ForallP 4 (ExistsPoly $
    ForallP 5 (ExistsPoly $
    ForallP 6 (ExistsPoly $
      MonoP $ Mono $ (anything :: Type ((a -> b) -> (b -> c) -> a -> c))
    :: forall c. A Type c => ExistsPoly Type c)
    :: forall b. A Type b => ExistsPoly Type b)
    :: forall a. A Type a => ExistsPoly Type a)
  ,
    ForallP 7 (ExistsPoly $
    ForallP 8 (ExistsPoly $
      MonoP $ Mono $ (anything :: Type ((a -> b) -> (b -> a) -> a -> a))
    :: forall b. A Type b => ExistsPoly Type b)
    :: forall a. A Type a => ExistsPoly Type a)
  ]

uast_func_examples :: [UAst]
uast_func_examples = [
    -- (|>)
    LambdaUA "a" (ForallUPT "a" $ MonoUPT $ VarUMT "a") $
    LambdaUA "f" (ForallUPT "b" $ MonoUPT $ VarUMT "a" `ArrowUMT` VarUMT "b") $
    VarUA "f" `AppUA` VarUA "a"
  ,
    -- (.)
    LambdaUA "f" (
      ForallUPT "b" $ ForallUPT "c" $ MonoUPT $
      VarUMT "b" `ArrowUMT` VarUMT "c"
    ) $ LambdaUA "g" (
      ForallUPT "a" $ MonoUPT $
      VarUMT "a" `ArrowUMT` VarUMT "b"
    ) $ LambdaUA "x" (
      MonoUPT $ VarUMT "a"
    ) $ VarUA "f" `AppUA` (VarUA "g" `AppUA` VarUA "x")
  ,
    -- (;)
    LambdaUA "f" (
      ForallUPT "a" $ ForallUPT "b" $ MonoUPT $
      VarUMT "a" `ArrowUMT` VarUMT "b"
    ) $ LambdaUA "g" (
      ForallUPT "c" $ MonoUPT $
      VarUMT "b" `ArrowUMT` VarUMT "c"
    ) $ LambdaUA "x" (
      MonoUPT $ VarUMT "a"
    ) $ VarUA "g" `AppUA` (VarUA "f" `AppUA` VarUA "x")
  ]

uast_int_examples = [
    -- 8
    (uast_func_examples !! 0)
      `AppUA` (LiteralUA 5) `AppUA` (AddUA `AppUA` (LiteralUA 3))
  ,
    -- 8
    LetUA "app" (uast_func_examples !! 0) $
      VarUA "app" `AppUA` (LiteralUA 5) `AppUA` (AddUA `AppUA` (LiteralUA 3))
  ]

testTypesImplDev = map show polytype_examples == [
    "forall a1 b2. a -> (a -> b) -> b",
    "forall a3. (a -> a) -> (a -> a) -> a -> a",
    "forall a4 b5 c6. (a -> b) -> (b -> c) -> a -> c",
    "forall a7 b8. (a -> b) -> (b -> a) -> a -> a"
  ]

testTypesImplRel = map show polytype_examples == [
    "forall a b. a -> (a -> b) -> b",
    "forall a. (a -> a) -> (a -> a) -> a -> a",
    "forall a b c. (a -> b) -> (b -> c) -> a -> c",
    "forall a b. (a -> b) -> (b -> a) -> a -> a"
  ]

testTypesImpl = testTypesImplDev || testTypesImplRel
