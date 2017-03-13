{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.Examples where
import Base.Pervasives
import Base.UAst
import qualified Base.Letters as Letters
import Base.LettersImpl
import Base.Types
import Base.TypesImpl
import Base.Symbol
import Base.SymbolImpl

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

uast_int_examples :: [UAst]
uast_int_examples = [
    -- 8
    (uast_func_examples !! 0)
      `AppUA` (LiteralUA 5) `AppUA` (AddUA `AppUA` (LiteralUA 3))
  ,
    -- 8
    LetUA "app" (uast_func_examples !! 0) $
      VarUA "app" `AppUA` (LiteralUA 5) `AppUA` (AddUA `AppUA` (LiteralUA 3))
  ]

monorecordtype_examples :: [Mono RecordType]
monorecordtype_examples =
  let
    a = FieldName $ Letters.A_UL `ConsSYM` NilSYM
    b = FieldName $ Letters.B_LL `ConsSYM` NilSYM
    fun = FieldName $ Letters.F_LL `ConsSYM` Letters.U_LL
      `ConsSYM` Letters.N_LL `ConsSYM` NilSYM
    nest = FieldName $ Letters.N_LL `ConsSYM` Letters.E_LL
      `ConsSYM` Letters.S_LL `ConsSYM` Letters.T_LL `ConsSYM` NilSYM
  in [
    Mono $ (a, IntT) `ConsRT` (b, IntT)
      `ConsRT` (fun, IntT `ArrowT` IntT) `ConsRT` NilRT,
    (
      case monorecordtype_examples !! 0 of
        Mono inner -> Mono $
          (nest, RecordT inner) `ConsRT` (a, IntT) `ConsRT` NilRT
    ),
    Mono $ NilRT
  ]

monorecord_examples :: [Mono Record]
monorecord_examples =
  let
    a = FieldName $ Letters.A_UL `ConsSYM` NilSYM
    b = FieldName $ Letters.B_LL `ConsSYM` NilSYM
    fun = FieldName $ Letters.F_LL `ConsSYM` Letters.U_LL
      `ConsSYM` Letters.N_LL `ConsSYM` NilSYM
    nest = FieldName $ Letters.N_LL `ConsSYM` Letters.E_LL
      `ConsSYM` Letters.S_LL `ConsSYM` Letters.T_LL `ConsSYM` NilSYM
  in [
    Mono $ (a, 5::Int) `ConsRC` (b, 2::Int)
      `ConsRC` (fun, (+3) :: Int -> Int) `ConsRC` NilRC,
    (
      case monorecord_examples !! 0 of
        Mono inner -> Mono $
          (nest, inner) `ConsRC` (a, 5::Int) `ConsRC` NilRC
    ),
    Mono $ NilRC
  ]
