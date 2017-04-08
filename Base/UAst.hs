{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.UAst where
import Base.Pervasives

infixr `LambdaUA`
infixr `LetUA`
infixr `RecordConsUA`

type family UArgumentType (l :: Level) :: *
type instance UArgumentType Hi = Maybe UPolyType
type instance UArgumentType Lo = UPolyType

data UAst (l :: Level) = AddUA
  | LiteralUA Int
  | AppUA (UAst l) (UAst l)
  | LambdaUA (String, UArgumentType l) (UAst l)
  | VarUA String
  | LetUA (String, UAst l) (UAst l)
  | RecordNilUA
  | RecordConsUA (String, UAst l) (UAst l)
  | RecordGetUA String (UAst l)
deriving instance (Show (UArgumentType l) => Show (UAst l))

data UMonoType = IntUMT
  | ArrowUMT UMonoType UMonoType
  | VarUMT String
  | HasFieldUMT (String, UMonoType) UMonoType
  deriving (Show)

data UPolyType = ForallUPT String UPolyType | MonoUPT UMonoType
  deriving (Show)
