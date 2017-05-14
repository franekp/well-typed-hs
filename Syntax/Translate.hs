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

transPIdent :: PIdent -> String
transPIdent (PIdent ((_, _), str)) = str

transModule :: Pos Module -> UAst Lo
transModule (Pos _ _ x) = case x of
  Module defs  -> transExpr $ dummyPos $ ELet defs $ dummyPos $ EVar (PIdent ((-1, -1), "main"))

transDef :: Pos Def -> Binding
transDef (Pos p q x) = case x of
  DVar id args expr -> BVar (transPIdent id, transExpr $ Pos p q $ ELambda args expr)
  DOpVar anyop args expr -> BVar (transAnyOp anyop, transExpr $ Pos p q $ ELambda args expr)
  DOpen id -> BOpenModule (transPIdent id)
  DType id tt -> BTypeDef (transPIdent id) (transType tt)

transAnyOp :: Pos AnyOp -> String
transAnyOp (Pos p q x) = case x of
  AnyAppOp (RAppOp ((_, _), str)) -> str
  AnyPipeOp (LPipeOp ((_, _), str)) -> str
  AnyAndOp (LAndOp ((_, _), str)) -> str
  AnyAssignOp (RAssignOp ((_, _), str)) -> str
  AnyRelOp (RelOp ((_, _), str)) -> str
  AnyAddOp (LAddOp ((_, _), str)) -> str
  AnyMulOp (LMulOp ((_, _), str)) -> str
  AnyExpOp (RExpOp ((_, _), str)) -> str
  AnyComposeOp (RComposeOp ((_, _), str)) -> str

transExpr :: Pos Expr -> UAst Lo
transExpr (Pos p q x) = case x of
  ELet [] expr -> transExpr expr
  ELet (h:t) expr -> case transDef h of
    BVar b -> LetUA b $ transExpr $ Pos p q $ ELet t expr
    BOpenModule m -> OpenUA m $ transExpr $ Pos p q $ ELet t expr
    BTypeDef name tt -> TypeDefUA (name, tt) $ transExpr $ Pos p q $ ELet t expr
  ELetRec defs expr -> failure x
  ELambda [] expr -> transExpr expr
  ELambda (h:t) expr -> LambdaUA (transArg h) $ transExpr $ Pos p q $ ELambda t expr
  EObject [] -> RecordNilUA
  EObject (h:t) -> case transDef h of
    BVar b -> RecordConsUA b $ transExpr $ Pos p q $ EObject t
    BOpenModule m -> error "opening modules not allowed inside records"
    BTypeDef _ _ -> error "defining types not allowed inside records"

  EBlock [] -> VarUA "returnIO" `AppUA` VarUA "unit"
  EBlock (Pos _ p (BlockExprId expr):t) ->
    VarUA "nextIO" `AppUA` transExpr expr `AppUA` transExpr (Pos p q $ EBlock t)
  EBlock (Pos _ p (BlockExprBind id tt expr):t) ->
    VarUA "bindIO" `AppUA` transExpr expr `AppUA` (LambdaUA (transPIdent id, transTypeScheme tt) (transExpr $ Pos p q $ EBlock t))
  EBlock (Pos _ p (BlockExprDef id expr):t) ->
    LetUA (transPIdent id, transExpr expr) $ transExpr (Pos p q $ EBlock t)
  EList [] -> VarUA "nil"
  EList (h:t) -> VarUA "cons" `AppUA` transListItem h `AppUA` transExpr (Pos p q $ EList t)
  ETuple a [b] -> VarUA "pair" `AppUA` transListItem a `AppUA` transListItem b
  ETuple a [b, c] -> VarUA "triple" `AppUA` transListItem a `AppUA` transListItem b `AppUA` transListItem c
  ETuple listitem listitems -> failure x

  ERecord [] -> RecordNilUA
  ERecord (h:t) -> case transRecordItem h of
    BVar b -> RecordConsUA b $ transExpr $ Pos p q $ ERecord t
    BOpenModule m -> error "opening modules not allowed inside records"
    BTypeDef _ _ -> error "defining types not allowed inside records"

  ERecordUpdate id recorditems  -> failure x
  ECoerce expr type'  -> failure x
  EUnit -> VarUA "unit"
  EIf expr1 expr2 expr3  -> VarUA "ifThenElse" `AppUA` transExpr expr1 `AppUA` transExpr expr2 `AppUA` transExpr expr3

  EAppOp e1 (RAppOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EPipeOp e1 (LPipeOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EAndOp e1 (LAndOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EAssignOp e1 (RAssignOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EAddOp e1 (LAddOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EMulOp e1 (LMulOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EExpOp e1 (RExpOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2
  EComposeOp e1 (RComposeOp ((_, _), op)) e2 -> VarUA op `AppUA` transExpr e1 `AppUA` transExpr e2

  ERelOp expr [] -> VarUA "True"
  ERelOp expr (Pos pp qq (RelExpr (RelOp ((_, _), op)) e) : relexprs) ->
    VarUA "&&"
    `AppUA` (VarUA op `AppUA` transExpr expr `AppUA` transExpr e)
    `AppUA` (transExpr $ Pos p q $ ERelOp e relexprs)

  EBackticks e1 func e2 ->
    VarUA (transPIdent func) `AppUA` transExpr e1 `AppUA` transExpr e2
  EApp e1 e2 -> transExpr e1 `AppUA` transExpr e2
  EFieldAccess expr id -> RecordGetUA (transPIdent id) (transExpr expr)
  EVar id -> VarUA (transPIdent id)
  EOpVar anyop -> VarUA (transAnyOp anyop)
  EPartialOp anyop expr -> VarUA "flip" `AppUA` VarUA (transAnyOp anyop) `AppUA` transExpr expr
  EInt (PInteger ((_, _), n)) -> LiteralUA (read n :: Int)
  EString (PString ((_, _), str)) -> StringUA $ Syntax.LexGrammar.unescapeInitTail str

transListItem :: Pos ListItem -> UAst Lo
transListItem (Pos _ _ x) = case x of
  ListItem expr  -> transExpr expr

transRecordItem :: Pos RecordItem -> Binding
transRecordItem (Pos _ _ x) = case x of
  RecordItem def  -> transDef def

transArg :: Pos Arg -> (String, UPolyType)
transArg (Pos _ _ x) = case x of
  Arg id typescheme -> (transPIdent id, transTypeScheme typescheme)
  ArgUnit -> ("__unit__", MonoUPT UnitUMT)

transTypeV :: Pos TypeV -> String
transTypeV (Pos _ _ x) = case x of
  TypeV id  -> transPIdent id

transTypeScheme :: Pos TypeScheme -> UPolyType
transTypeScheme (Pos p q x) = case x of
  TypeScheme [] type' -> MonoUPT $ transType type'
  TypeScheme (h:t) type' -> ForallUPT (transTypeV h) $ transTypeScheme $ Pos p q $ TypeScheme t type'

transType :: Pos Type -> UMonoType
transType (Pos p q x) = case x of
  TArrow type1 type2  -> transType type1 `ArrowUMT` transType type2
  TRecord id [] -> VarUMT $ transPIdent id
  TRecord id (h:t) -> HasFieldUMT (transFieldAnnotation h) $ transType $ Pos p q $ TRecord id t
  TExactRecord li -> let
      helper1 :: [Pos FieldAnnotation] -> UMonoType
      helper1 [] = RecordNilUMT
      helper1 (h:t) = RecordConsUMT (transFieldAnnotation h) $ helper1 t
      helper2 :: UMonoType -> [Pos FieldAnnotation] -> UMonoType
      helper2 inner [] = inner
      helper2 inner (h:t) = HasFieldUMT (transFieldAnnotation h) $ helper2 inner t
    in helper2 (helper1 li) li
  TInt  -> IntUMT
  TVar id  -> VarUMT $ transPIdent id

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

transFieldAnnotation :: Pos FieldAnnotation -> (String, UMonoType)
transFieldAnnotation (Pos _ _ x) = case x of
  FieldAnnotation id type'  -> (transPIdent id, transType type')
