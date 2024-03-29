{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Base.UAst where
import Base.Pervasives

infixr `LambdaUA`
infixr `LetUA`
infixr `RecordConsUA`

type SourceInfo = ((Int, Int), (Int, Int), (String, String))

show_source :: SourceInfo -> String
show_source src@((line1, col1), (line2, col2), (path, _)) = position ++ "\n" ++ msg where
  position' = if line1 == line2 then
      path ++ ":" ++ show line1 ++ ":" ++ show col1 ++ "-" ++ show col2 ++ ":"
    else
      path ++ ":" ++ show line1 ++ "-" ++ show line2 ++ ":"
  position = "\ESC[33m" ++ position' ++ "\ESC[0m"
  msg = show_source' src

show_source' ((line1, col1), (line2, col2), (_, src)) =
    if line1 < 0 || col1 < 0 || line2 < 0 || col2 < 0 then "<unknown>" else
    if line1 == line2 then
      show_one_line
    else
      show_block
  where
    underline =
      replicate (max 0 $ col1 - 1) ' ' ++ replicate (max 1 $ col2 - col1) '^'
    show_one_line =
      "\ESC[0m" ++ ("":lines src) !! line1 ++ "\ESC[0m\n\ESC[31;1m" ++ underline ++ "\ESC[0m"
    show_block =
      concat $ drop line1 $ take line2 $ "":lines src

data UAstImpl = AddUA
  | LiteralUA Int
  | StringUA String
  | AppUA UAst UAst
  | LambdaUA (String, UPolyType) UAst
  | VarUA String
  | LetUA (String, UAst) UAst
  | RecordNilUA
  | RecordConsUA (String, UAst) UAst
  | RecordGetUA String UAst
  | OpenUA String UAst
  | TypeDefUA (String, UMonoType) UAst
  deriving Eq

data UAst = UAst SourceInfo UAstImpl
  deriving Eq
instance Show UAst where
  show (UAst src _) = show_source src

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
  deriving Eq

data UMonoType = UMonoType SourceInfo UMonoTypeImpl
  deriving Eq
instance Show UMonoType where
  show (UMonoType src _) = show_source src

data UPolyType = UPolyType [String] UMonoType
  deriving Eq

newtype ExtModuleTypeEnv = ExtModuleTypeEnv [(String, [(String, UPolyType)])]
