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

dummyPos :: a -> Pos a
dummyPos = Pos (-1, -1) (-1, -1)

transPIdent :: String -> PIdent -> String
transPIdent s (PIdent ((_, _), str)) = str

transModule :: String -> Pos Module -> UAst Lo
transModule s (Pos _ _ x) = case x of
  Module defs  -> transExpr s $ dummyPos $ ELet defs $ dummyPos $ EVar (PIdent ((-1, -1), "main"))

transDef :: String -> Pos Def -> Binding
transDef s (Pos p q x) = case x of
  DVar id args expr -> BVar (transPIdent s id, transExpr s $ Pos p q $ ELambda args expr)
  DOpVar anyop args expr -> BVar (transAnyOp s anyop, transExpr s $ Pos p q $ ELambda args expr)
  DOpen id -> BOpenModule (transPIdent s id)
  DType id tt -> BTypeDef (transPIdent s id) (transType s tt)

transAnyOp :: String -> Pos AnyOp -> String
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

transExpr :: String -> Pos Expr -> UAst Lo
transExpr s (Pos p q x) = case x of
  ELet [] expr -> transExpr s expr
  ELet (h:t) expr -> case transDef s h of
    BVar b -> LetUA b $ transExpr s $ Pos p q $ ELet t expr
    BOpenModule m -> OpenUA m $ transExpr s $ Pos p q $ ELet t expr
    BTypeDef name tt -> TypeDefUA (name, tt) $ transExpr s $ Pos p q $ ELet t expr
  ELetRec defs expr -> failure x
  ELambda [] expr -> transExpr s expr
  ELambda (h:t) expr -> LambdaUA (transArg s h) $ transExpr s $ Pos p q $ ELambda t expr
  EObject [] -> RecordNilUA
  EObject (h:t) -> case transDef s h of
    BVar b -> RecordConsUA b $ transExpr s $ Pos p q $ EObject t
    BOpenModule m -> error "opening modules not allowed inside records"
    BTypeDef _ _ -> error "defining types not allowed inside records"

  EBlock [] -> VarUA "returnIO" `AppUA` VarUA "unit"
  EBlock (Pos _ p (BlockExprId expr):t) ->
    VarUA "nextIO" `AppUA` transExpr s expr `AppUA` transExpr s (Pos p q $ EBlock t)
  EBlock (Pos _ p (BlockExprBind id tt expr):t) ->
    VarUA "bindIO" `AppUA` transExpr s expr `AppUA` (LambdaUA (transPIdent s id, transTypeScheme s tt) (transExpr s $ Pos p q $ EBlock t))
  EBlock (Pos _ p (BlockExprDef id expr):t) ->
    LetUA (transPIdent s id, transExpr s expr) $ transExpr s (Pos p q $ EBlock t)
  EList [] -> VarUA "nil"
  EList (h:t) -> VarUA "cons" `AppUA` transListItem s h `AppUA` transExpr s (Pos p q $ EList t)
  ETuple a [b] -> VarUA "pair" `AppUA` transListItem s a `AppUA` transListItem s b
  ETuple a [b, c] -> VarUA "triple" `AppUA` transListItem s a `AppUA` transListItem s b `AppUA` transListItem s c
  ETuple listitem listitems -> failure x

  ERecord [] -> RecordNilUA
  ERecord (h:t) -> case transRecordItem s h of
    BVar b -> RecordConsUA b $ transExpr s $ Pos p q $ ERecord t
    BOpenModule m -> error "opening modules not allowed inside records"
    BTypeDef _ _ -> error "defining types not allowed inside records"

  ERecordUpdate id recorditems  -> failure x
  ECoerce expr type'  -> failure x
  EUnit -> VarUA "unit"
  EIf expr1 expr2 expr3  -> VarUA "ifThenElse" `AppUA` transExpr s expr1 `AppUA` transExpr s expr2 `AppUA` transExpr s expr3

  EAppOp e1 (RAppOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EPipeOp e1 (LPipeOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EAndOp e1 (LAndOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EAssignOp e1 (RAssignOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EAddOp e1 (LAddOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EMulOp e1 (LMulOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EExpOp e1 (RExpOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EComposeOp e1 (RComposeOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr s e1 `AppUA` transExpr s e2

  ERelOp expr [] -> VarUA "True"
  ERelOp expr (Pos pp qq (RelExpr (RelOp ((_, _), op)) e) : relexprs) ->
    VarUA "&&"
    `AppUA` (VarUA op `AppUA` transExpr s expr `AppUA` transExpr s e)
    `AppUA` (transExpr s $ Pos p q $ ERelOp e relexprs)

  EBackticks e1 func e2 ->
    VarUA (transPIdent s func) `AppUA` transExpr s e1 `AppUA` transExpr s e2
  EApp e1 e2 -> transExpr s e1 `AppUA` transExpr s e2
  EFieldAccess expr id -> RecordGetUA (transPIdent s id) (transExpr s expr)
  EVar id -> VarUA (transPIdent s id)
  EOpVar anyop -> VarUA (transAnyOp s anyop)
  EPartialOp anyop expr -> VarUA "flip" `AppUA` VarUA (transAnyOp s anyop) `AppUA` transExpr s expr
  EInt (PInteger ((_, _), n)) -> LiteralUA (read n :: Int)
  EString (PString ((_, _), str)) -> StringUA $ Syntax.LexGrammar.unescapeInitTail str

transListItem :: String -> Pos ListItem -> UAst Lo
transListItem s (Pos _ _ x) = case x of
  ListItem expr  -> transExpr s expr

transRecordItem :: String -> Pos RecordItem -> Binding
transRecordItem s (Pos _ _ x) = case x of
  RecordItem def  -> transDef s def

transArg :: String -> Pos Arg -> (String, UPolyType)
transArg s (Pos _ _ x) = case x of
  Arg id typescheme -> (transPIdent s id, transTypeScheme s typescheme)
  ArgUnit -> ("__unit__", MonoUPT UnitUMT)

transTypeV :: String -> Pos TypeV -> String
transTypeV s (Pos _ _ x) = case x of
  TypeV id  -> transPIdent s id

transTypeScheme :: String -> Pos TypeScheme -> UPolyType
transTypeScheme s (Pos p q x) = case x of
  TypeScheme [] type' -> MonoUPT $ transType s type'
  TypeScheme (h:t) type' -> ForallUPT (transTypeV s h) $ transTypeScheme s $ Pos p q $ TypeScheme t type'

transType :: String -> Pos Type -> UMonoType
transType s (Pos p q x) = case x of
  TArrow type1 type2  -> transType s type1 `ArrowUMT` transType s type2
  TRecord id [] -> VarUMT $ transPIdent s id
  TRecord id (h:t) -> HasFieldUMT (transFieldAnnotation s h) $ transType s $ Pos p q $ TRecord id t
  TExactRecord li -> let
      helper1 :: [Pos FieldAnnotation] -> UMonoType
      helper1 [] = RecordNilUMT
      helper1 (h:t) = RecordConsUMT (transFieldAnnotation s h) $ helper1 t
      helper2 :: UMonoType -> [Pos FieldAnnotation] -> UMonoType
      helper2 inner [] = inner
      helper2 inner (h:t) = HasFieldUMT (transFieldAnnotation s h) $ helper2 inner t
    in helper2 (helper1 li) li
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

transFieldAnnotation :: String -> Pos FieldAnnotation -> (String, UMonoType)
transFieldAnnotation s (Pos _ _ x) = case x of
  FieldAnnotation id type'  -> (transPIdent s id, transType s type')
