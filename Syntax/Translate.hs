{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Syntax.Translate where

import Syntax.AbsGrammar
import Syntax.ErrM

import Base.Pervasives
import Base.UAst

data Result

data Binding = BVar (String, UAst Lo) | BOpenModule String

failure :: Show a => a -> b
failure x = error $ "Undefined case: " ++ show x

transIdent :: Ident -> String
transIdent x = case x of
  Ident str  -> str

transModule :: Module -> UAst Lo
transModule x = case x of
  Module defs  -> transExpr $ ELet defs $ EVar (Ident "main")

transDef :: Def -> Binding
transDef x = case x of
  DVar id args expr -> BVar (transIdent id, transExpr $ ELambda args expr)
  DOpVar anyop args expr -> BVar (transAnyOp anyop, transExpr $ ELambda args expr)
  DOpen id -> BOpenModule (transIdent id)

transAnyOp :: AnyOp -> String
transAnyOp x = case x of
  AnyAppOp (RAppOp str) -> str
  AnyPipeOp (LPipeOp str) -> str
  AnyAndOp (LAndOp str) -> str
  AnyAssignOp (RAssignOp str) -> str
  AnyRelOp (RelOp str) -> str
  AnyAddOp (LAddOp str) -> str
  AnyMulOp (LMulOp str) -> str
  AnyExpOp (RExpOp str) -> str
  AnyComposeOp (RComposeOp str) -> str

transExpr :: Expr -> UAst Lo
transExpr x = case x of
  ELet [] expr -> transExpr expr
  ELet (h:t) expr -> case transDef h of
    BVar b -> LetUA b $ transExpr $ ELet t expr
    BOpenModule m -> OpenUA m $ transExpr $ ELet t expr
  ELetRec defs expr -> failure x
  ELambda [] expr -> transExpr expr
  ELambda (h:t) expr -> LambdaUA (transArg h) $ transExpr $ ELambda t expr
  EObject [] -> RecordNilUA
  EObject (h:t) -> case transDef h of
    BVar b -> RecordConsUA b $ transExpr $ EObject t
    BOpenModule m -> error "opening modules not allowed inside records"

  EBlock [] -> VarUA "returnIO" `AppUA` VarUA "unit"
  EBlock (BlockExprId expr:t) ->
    VarUA "nextIO" `AppUA` transExpr expr `AppUA` transExpr (EBlock t)
  EBlock (BlockExprBind id tt expr:t) ->
    VarUA "bindIO" `AppUA` transExpr expr `AppUA` (LambdaUA (transIdent id, transTypeScheme tt) (transExpr $ EBlock t))
  EBlock (BlockExprDef id expr:t) ->
    LetUA (transIdent id, transExpr expr) $ transExpr (EBlock t)
  EList [] -> VarUA "nil"
  EList (h:t) -> VarUA "cons" `AppUA` transListItem h `AppUA` transExpr (EList t)
  ETuple a [b] -> VarUA "pair" `AppUA` transListItem a `AppUA` transListItem b
  ETuple a [b, c] -> VarUA "triple" `AppUA` transListItem a `AppUA` transListItem b `AppUA` transListItem c
  ETuple listitem listitems -> failure x

  ERecord [] -> RecordNilUA
  ERecord (h:t) -> case transRecordItem h of
    BVar b -> RecordConsUA b $ transExpr $ ERecord t
    BOpenModule m -> error "opening modules not allowed inside records"

  ERecordUpdate id recorditems  -> failure x
  ECoerce expr type'  -> failure x
  EIf expr1 expr2 expr3  -> VarUA "ifThenElse" `AppUA` transExpr expr1 `AppUA` transExpr expr2 `AppUA` transExpr expr3

  EAppOp e1 (RAppOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EPipeOp e1 (LPipeOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EAndOp e1 (LAndOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EAssignOp e1 (RAssignOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EAddOp e1 (LAddOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EMulOp e1 (LMulOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EExpOp e1 (RExpOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EComposeOp e1 (RComposeOp op) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2

  ERelOp expr [] -> VarUA "True"
  ERelOp expr (RelExpr (RelOp op) e : relexprs) ->
    VarUA "&&"
    `AppUA` (VarUA op `AppUA` transExpr expr `AppUA` transExpr e)
    `AppUA` (transExpr $ ERelOp e relexprs)

  EBackticks e1 func e2 ->
    VarUA (transIdent func) `AppUA` transExpr e1 `AppUA` transExpr e2
  EApp e1 e2 -> transExpr e1 `AppUA` transExpr e2
  EFieldAccess expr id -> RecordGetUA (transIdent id) (transExpr expr)
  EVar id -> VarUA (transIdent id)
  EOpVar anyop -> VarUA (transAnyOp anyop)
  EInt n -> LiteralUA (fromIntegral n)
  EString str  -> StringUA str

transListItem :: ListItem -> UAst Lo
transListItem x = case x of
  ListItem expr  -> transExpr expr

transRecordItem :: RecordItem -> Binding
transRecordItem x = case x of
  RecordItem def  -> transDef def

transArg :: Arg -> (String, UPolyType)
transArg x = case x of
  Arg id typescheme -> (transIdent id, transTypeScheme typescheme)

transTypeV :: TypeV -> String
transTypeV x = case x of
  TypeV id  -> transIdent id

transTypeScheme :: TypeScheme -> UPolyType
transTypeScheme x = case x of
  TypeScheme [] type' -> MonoUPT $ transType type'
  TypeScheme (h:t) type' -> ForallUPT (transTypeV h) $ transTypeScheme $ TypeScheme t type'

transType :: Type -> UMonoType
transType x = case x of
  TArrow type1 type2  -> transType type1 `ArrowUMT` transType type2
  TRecord id [] -> VarUMT $ transIdent id
  TRecord id (h:t) -> HasFieldUMT (transFieldAnnotation h) $ transType $ TRecord id t
  TInt  -> IntUMT
  TVar id  -> VarUMT $ transIdent id

  TBool -> BoolUMT
  TMaybe a -> MaybeUMT (transType a)
  TEither a b -> EitherUMT (transType a) (transType b)
  TChar -> CharUMT
  TList a -> ListUMT (transType a)
  T_IO a -> IO_UMT (transType a)
  TDynamic -> DynamicUMT
  TUnit -> UnitUMT
  TPair a b -> PairUMT (transType a) (transType b)
  TTriple a b c -> TripleUMT (transType a) (transType b) (transType c)

transFieldAnnotation :: FieldAnnotation -> (String, UMonoType)
transFieldAnnotation x = case x of
  FieldAnnotation id type'  -> (transIdent id, transType type')
