{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.UAst where
import Base.Pervasives

infixr `LambdaUA`
infixr `LetUA`
infixr `RecordConsUA`

type SourceInfo = ((Int, Int), (Int, Int), String)

type family UArgumentType (l :: Level) :: *
type instance UArgumentType Hi = Maybe UPolyType
type instance UArgumentType Lo = UPolyType

data UAstImpl (l :: Level) = AddUA
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
  | TypeDefUA (String, UMonoType) (UAst l)
deriving instance (Show (UArgumentType l) => Show (UAstImpl l))

data UAst (l :: Level) = UAst SourceInfo (UAstImpl l)
deriving instance (Show (UArgumentType l) => Show (UAst l))

data UMonoTypeImpl = ArrowUMT UMonoType UMonoType
  | IntUMT
  | VarUMT String
  | HasFieldUMT (String, UMonoType) UMonoType
  | RecordConsUMT (String, UMonoType) UMonoType
  | RecordNilUMT
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

data UMonoType = UMonoType SourceInfo UMonoTypeImpl
  deriving (Show)

data UPolyTypeImpl = ForallUPT String UPolyType | MonoUPT UMonoType
  deriving (Show)

data UPolyType = UPolyType SourceInfo UPolyTypeImpl
  deriving (Show)
