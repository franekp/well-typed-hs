{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.UAst where

data UAst = AddUA
  | LiteralUA Int
  | AppUA UAst UAst
  | LambdaUA String UPolyType UAst
  | VarUA String
  | LetUA String UAst UAst

data UMonoType = IntUMT | ArrowUMT UMonoType UMonoType | VarUMT String
data UPolyType = ForallUPT String UPolyType | MonoUPT UMonoType
