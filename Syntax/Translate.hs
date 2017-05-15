{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Syntax.Translate where

import Syntax.AbsGrammar
import qualified Syntax.LexGrammar
import Syntax.ErrM

import Base.Pervasives
import Base.UAst

data Result

data Binding = BVar (String, UAst Lo) | BOpenModule String | BTypeDef String UMonoType

failure :: Show a => a -> b
failure x = error $ "Undefined case: " ++ show x

transPIdent :: (String, String) -> PIdent -> String
transPIdent s (PIdent ((_, _), str)) = str

transModule :: (String, String) -> Pos Module -> UAst Lo
transModule s (Pos p q x) = case x of
  Module defs  -> transExpr s $ Pos p q $ ELet defs $ Pos p q $ EVar (PIdent ((-1, -1), "main"))

transDef :: (String, String) -> Pos Def -> Binding
transDef s (Pos p q x) = case x of
  DVar id args expr -> BVar (transPIdent s id, transExpr s $ Pos p q $ ELambda args expr)
  DOpVar anyop args expr -> BVar (transAnyOp s anyop, transExpr s $ Pos p q $ ELambda args expr)
  DOpen id -> BOpenModule (transPIdent s id)
  DType id tt -> BTypeDef (transPIdent s id) (transType s tt)

transAnyOp :: (String, String) -> Pos AnyOp -> String
transAnyOp s (Pos p q x) = case x of
  AnyAppOp (RAppOp ((_, _), str)) -> str
  AnyPipeOp (LPipeOp ((_, _), str)) -> str
  AnyAndOp (LAndOp ((_, _), str)) -> str
  AnyAssignOp (RAssignOp ((_, _), str)) -> str
  AnyRelOp (RelOp ((_, _), str)) -> str
  AnyAddOp (LAddOp ((_, _), str)) -> str
  AnyMulOp (LMulOp ((_, _), str)) -> str
  AnyExpOp (RExpOp ((_, _), str)) -> str
  AnyComposeOp (RComposeOp ((_, _), str)) -> str

transExpr :: (String, String) -> Pos Expr -> UAst Lo
transExpr s (Pos p q x) = UAst (p, q, s) $ case x of
  ELet [] expr -> case transExpr s expr of
    UAst _ res -> res
  ELet (h:t) expr -> case transDef s h of
    BVar b -> LetUA b $ transExpr s $ Pos (left_end t) q $ ELet t expr
    BOpenModule m -> OpenUA m $ transExpr s $ Pos (left_end t) q $ ELet t expr
    BTypeDef name tt -> TypeDefUA (name, tt) $ transExpr s $ Pos (left_end t) q $ ELet t expr
  ELetRec defs expr -> failure x
  ELambda [] expr -> case transExpr s expr of
    UAst _ res -> res
  ELambda (h:t) expr -> LambdaUA (transArg s h) $ transExpr s $ Pos (left_end t) q $ ELambda t expr
  EObject [] -> RecordNilUA
  EObject (h:t) -> case transDef s h of
    BVar b -> RecordConsUA b $ transExpr s $ Pos (left_end t) q $ EObject t
    BOpenModule m -> error "opening modules not allowed inside records"
    BTypeDef _ _ -> error "defining types not allowed inside records"

  EBlock [] -> (UAst (p, q, s) $ VarUA "returnIO") `AppUA` (UAst (p, q, s) $ VarUA "unit")
  EBlock (Pos pp qq (BlockExprId expr):t) ->
    let
      varUA = UAst (pp, qq, s) . VarUA
      appUA = (UAst (pp, qq, s) .) . AppUA
    in
      varUA "nextIO" `appUA` transExpr s expr `AppUA` transExpr s (Pos (left_end t) q $ EBlock t)
  EBlock (Pos pp qq (BlockExprBind id tt expr):t) ->
    let
      varUA = UAst (pp, qq, s) . VarUA
      appUA = (UAst (pp, qq, s) .) . AppUA
      lambdaUA = (UAst (p, q, s) .) . LambdaUA
    in
      varUA "bindIO" `appUA` transExpr s expr `AppUA` (lambdaUA (transPIdent s id, transTypeScheme s tt) (transExpr s $ Pos (left_end t) q $ EBlock t))
  EBlock (Pos pp qq (BlockExprDef id expr):t) ->
    LetUA (transPIdent s id, transExpr s expr) $ transExpr s (Pos (left_end t) q $ EBlock t)
  EList [] -> VarUA "nil"
  EList (h:t) -> let
      varUA = UAst (p, left_end h, s) . VarUA
      appUA = (UAst (p, right_end h, s) .) . AppUA
    in
      varUA "cons" `appUA` transListItem s h `AppUA` transExpr s (Pos (left_end t) q $ EList t)
  ETuple a [b] -> let
      varUA = UAst (p, left_end a, s) . VarUA
      appUA = (UAst (p, right_end a, s) .) . AppUA
    in
      varUA "pair" `appUA` transListItem s a `AppUA` transListItem s b
  ETuple a [b, c] -> let
      varUA = UAst (p, left_end a, s) . VarUA
      appUA_1 = (UAst (p, right_end a, s) .) . AppUA
      appUA_2 = (UAst (p, right_end b, s) .) . AppUA
    in
      varUA "triple" `appUA_1` transListItem s a `appUA_2` transListItem s b `AppUA` transListItem s c
  ETuple listitem listitems -> failure x

  ERecord [] -> RecordNilUA
  ERecord (h:t) -> case transRecordItem s h of
    BVar b -> RecordConsUA b $ transExpr s $ Pos (left_end t) q $ ERecord t
    BOpenModule m -> error "opening modules not allowed inside records"
    BTypeDef _ _ -> error "defining types not allowed inside records"

  ERecordUpdate id recorditems  -> failure x
  ECoerce expr type' -> failure x
  EUnit -> VarUA "unit"
  EIf expr1 expr2 expr3 -> let
      varUA = UAst (p, left_end expr1, s) . VarUA
      appUA_1 = (UAst (p, right_end expr1, s) .) . AppUA
      appUA_2 = (UAst (p, right_end expr2, s) .) . AppUA
    in
      varUA "ifThenElse" `appUA_1` transExpr s expr1 `appUA_2` transExpr s expr2 `AppUA` transExpr s expr3

  EAppOp e1 tok@(RAppOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2
  EPipeOp e1 tok@(LPipeOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2
  EAndOp e1 tok@(LAndOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2
  EAssignOp e1 tok@(RAssignOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2
  EAddOp e1 tok@(LAddOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2
  EMulOp e1 tok@(LMulOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2
  EExpOp e1 tok@(RExpOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2
  EComposeOp e1 tok@(RComposeOp (_, op)) e2 -> let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA = (UAst (p, right_end tok, s) .) . AppUA
    in
      varUA op `appUA` transExpr s e1 `AppUA` transExpr s e2

  ERelOp expr [] -> VarUA "True"
  ERelOp expr (Pos pp qq (RelExpr tok@(RelOp (_, op)) e) : relexprs) ->
    let
      varUA = UAst (left_end tok, right_end tok, s) . VarUA
      appUA_1 = (UAst (left_end expr, right_end e, s) .) . AppUA
      appUA_2 = (UAst (left_end expr, right_end tok, s) .) . AppUA
      appUA_3 = (UAst (left_end expr, right_end e, s) .) . AppUA
    in
      varUA "&&"
      `appUA_1` (varUA op `appUA_2` transExpr s expr `appUA_3` transExpr s e)
      `AppUA` (transExpr s $ Pos (left_end e) q $ ERelOp e relexprs)

  EBackticks e1 func e2 -> let
      varUA = UAst (left_end func, right_end func, s) . VarUA
      appUA = (UAst (p, right_end func, s) .) . AppUA
    in
      varUA (transPIdent s func) `appUA` transExpr s e1 `AppUA` transExpr s e2
  EApp e1 e2 -> transExpr s e1 `AppUA` transExpr s e2
  EFieldAccess expr id -> let
      lambdaUA = (UAst (left_end id, right_end id, s) .) . LambdaUA
    in
      (lambdaUA (
        "__record__",
        ForallUPT "r" $ ForallUPT "f" $ MonoUPT
          $ UMonoType (p, q, s) $ HasFieldUMT (transPIdent s id, UMonoType (p, q, s) $ VarUMT "f")
            $ UMonoType (p, q, s) $ VarUMT "r"
      ) $ UAst (p, q, s) $ RecordGetUA (transPIdent s id)
        (UAst (p, q, s) $ VarUA "__record__")
      ) `AppUA` (transExpr s expr)
  EVar id -> VarUA (transPIdent s id)
  EOpVar anyop -> VarUA (transAnyOp s anyop)
  EPartialOp anyop expr -> let
      varUA = UAst (left_end anyop, right_end anyop, s) . VarUA
      appUA = (UAst (p, right_end anyop, s) .) . AppUA
    in
      varUA "flip" `appUA` varUA (transAnyOp s anyop) `AppUA` transExpr s expr
  EInt (PInteger ((_, _), n)) -> LiteralUA (read n :: Int)
  EString (PString ((_, _), str)) -> StringUA $ Syntax.LexGrammar.unescapeInitTail str

transListItem :: (String, String) -> Pos ListItem -> UAst Lo
transListItem s (Pos _ _ x) = case x of
  ListItem expr  -> transExpr s expr

transRecordItem :: (String, String) -> Pos RecordItem -> Binding
transRecordItem s (Pos _ _ x) = case x of
  RecordItem def  -> transDef s def

transArg :: (String, String) -> Pos Arg -> (String, UPolyType)
transArg s (Pos p q x) = case x of
  Arg id typescheme -> (transPIdent s id, transTypeScheme s typescheme)
  ArgUnit -> ("__unit__", MonoUPT $ UMonoType (p, q, s) $ UnitUMT)

transTypeV :: (String, String) -> Pos TypeV -> String
transTypeV s (Pos _ _ x) = case x of
  TypeV id  -> transPIdent s id

transTypeScheme :: (String, String) -> Pos TypeScheme -> UPolyType
transTypeScheme s (Pos p q x) = case x of
  TypeScheme [] type' -> MonoUPT $ transType s type'
  TypeScheme (h:t) type' -> ForallUPT (transTypeV s h) $ transTypeScheme s $ Pos (left_end t) q $ TypeScheme t type'

transType :: (String, String) -> Pos Type -> UMonoType
transType s (Pos p q x) = UMonoType (p, q, s) $ case x of
  TArrow type1 type2  -> transType s type1 `ArrowUMT` transType s type2
  TRecord id [] -> VarUMT $ transPIdent s id
  TRecord id [h] -> HasFieldUMT (transFieldAnnotation s h) $ transType s $ Pos (left_end id) (right_end id) $ TRecord id []
  TRecord id (h:t) -> HasFieldUMT (transFieldAnnotation s h) $ transType s $ Pos (left_end t) q $ TRecord id t
  TExactRecord [] -> RecordNilUMT
  TExactRecord (h:t) -> RecordConsUMT (transFieldAnnotation s h) $ transType s $ Pos (left_end t) q $ TExactRecord t
  TInt  -> IntUMT
  TVar id  -> VarUMT $ transPIdent s id

  TBool -> BoolUMT
  TMaybe a -> MaybeUMT (transType s a)
  TEither a b -> EitherUMT (transType s a) (transType s b)
  TChar -> CharUMT
  TList a -> ListUMT (transType s a)
  T_IO a -> IO_UMT (transType s a)
  TDynamic -> DynamicUMT
  TUnit -> UnitUMT
  TPair a b -> PairUMT (transType s a) (transType s b)
  TTriple a b c -> TripleUMT (transType s a) (transType s b) (transType s c)

transFieldAnnotation :: (String, String) -> Pos FieldAnnotation -> (String, UMonoType)
transFieldAnnotation s (Pos _ _ x) = case x of
  FieldAnnotation id type'  -> (transPIdent s id, transType s type')
