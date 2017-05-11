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
  | StringUA String
  | AppUA (UAst l) (UAst l)
  | LambdaUA (String, UArgumentType l) (UAst l)
  | VarUA String
  | LetUA (String, UAst l) (UAst l)
  | RecordNilUA
  | RecordConsUA (String, UAst l) (UAst l)
  | RecordGetUA String (UAst l)
  | OpenUA String (UAst l)
deriving instance (Show (UArgumentType l) => Show (UAst l))

data UMonoType = ArrowUMT UMonoType UMonoType
  | IntUMT
  | VarUMT String
  | HasFieldUMT (String, UMonoType) UMonoType
  | BoolUMT
  | MaybeUMT UMonoType
  | EitherUMT UMonoType UMonoType
  | CharUMT
  | ListUMT UMonoType
  | IO_UMT UMonoType
  | DynamicUMT
  | UnitUMT
  | PairUMT UMonoType UMonoType
  | TripleUMT UMonoType UMonoType UMonoType
  deriving (Show)

data UPolyType = ForallUPT String UPolyType | MonoUPT UMonoType
  deriving (Show)
