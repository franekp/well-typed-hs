{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.UAst where

infixr `LambdaUA`
infixr `LetUA`
infixr `RecordConsUA`

data UAst = AddUA
  | LiteralUA Int
  | AppUA UAst UAst
  | LambdaUA (String, UPolyType) UAst
  | VarUA String
  | LetUA (String, UAst) UAst
  | RecordNilUA
  | RecordConsUA (String, UAst) UAst
  | RecordGetUA String UAst

data UMonoType = IntUMT | ArrowUMT UMonoType UMonoType | VarUMT String
data UPolyType = ForallUPT String UPolyType | MonoUPT UMonoType
