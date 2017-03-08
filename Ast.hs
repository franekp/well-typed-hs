{-# LANGUAGE EmptyDataDecls,
 MultiParamTypeClasses,
 GADTs,
 RankNTypes,
 StandaloneDeriving,
 DeriveDataTypeable,
 ScopedTypeVariables,
 FunctionalDependencies,
 OverlappingInstances,
 FlexibleInstances,
 FlexibleContexts,
 ExistentialQuantification,
 UndecidableInstances,
 TypeFamilies #-}

module Ast where

import Data.Void
import Data.Typeable (Typeable, Typeable1, Typeable2, cast)
import Data.List (foldl')
import Data.Bits (xor)

import Types
import Instances hiding (main)
import Unification

data Nil :: *
deriving instance Typeable Nil
data Cons :: * -> * -> *
deriving instance Typeable2 Cons

data Store :: * -> * where
  NilS :: Store Nil
  ConsS :: (Any h, Typeable t) => h -> Store t -> Store (Cons h t)

deriving instance Typeable1 Store

data Env :: * -> * where
  NilEN :: Env Nil
  ConsEN :: (Any h, Typeable t) => String -> Type h -> Env t -> Env (Cons h t)
  LetEN :: String -> Poly (Ast e) -> Env e -> Env e

deriving instance Typeable1 Env

data Ast :: * -> * -> * where
  AddA :: Typeable e => Ast e (Int -> Int -> Int)
  LiteralA :: Typeable e => Int -> Ast e Int

  VarA :: (Any a, Typeable e) => Ast (Cons a e) a
  LiftA :: (Any a, Any b, Typeable e) => Ast e a -> Ast (Cons b e) a

  LambdaA :: (Any a, Any b, Typeable e) => Ast (Cons a e) b -> Ast e (a -> b)

  ErrorA :: (Any a, Typeable e) => String -> Ast e a
  AppA :: (Any a, Any b, Typeable e) => Ast e (a -> b) -> Ast e a -> Ast e b

deriving instance Typeable2 Ast
deriving instance Show (Ast e a)

instance Show (Mono (Ast e)) where
  show (Mono a) = show a

eval :: Store e -> Ast e a -> a
eval s AddA = \x y -> x + y
eval s (LiteralA x) = x
eval (ConsS val _) VarA = val
eval (ConsS _ s) (LiftA a) = eval s a
eval s (LambdaA body) = \arg -> eval (ConsS arg s) body
eval s (ErrorA msg) = error msg
eval s (AppA fun arg) = eval s fun $ eval s arg

data Expr = AddE
  | LiteralE Int
  | AppE Expr Expr
  | LambdaE String PolyTypeExpr Expr
  | VarE String

data MonoTypeExpr = IntMTE | ArrowMTE MonoTypeExpr MonoTypeExpr | VarMTE String
data PolyTypeExpr = ForallPTE String PolyTypeExpr | MonoPTE MonoTypeExpr

newtype TypeEnv = TypeEnv [(String, Mono Type)]

lookup_type :: TypeEnv -> String -> Mono Type
lookup_type (TypeEnv []) var = error $ "unknown type variable: '" ++ var ++ "'"
lookup_type (TypeEnv ((name, tt):t)) var =
  if var == name then tt else lookup_type (TypeEnv t) var

update_typeenv :: TypeEnv -> String -> Mono Type -> TypeEnv
update_typeenv (TypeEnv li) name tt = TypeEnv $ (name, tt):li

typecheck_monotype :: TypeEnv -> MonoTypeExpr -> Mono Type
typecheck_monotype te IntMTE = Mono IntTT
typecheck_monotype te (a `ArrowMTE` b) =
  case (typecheck_monotype te a, typecheck_monotype te b) of
    (Mono a', Mono b') -> Mono $ a' `ArrowTT` b'
typecheck_monotype te (VarMTE var) = te `lookup_type` var

hash :: String -> Int
hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381

-- FIXME: this should be a monad generating unique ids, not hashes!
typecheck_polytype :: forall u. TypeEnv -> PolyTypeExpr
  -> (forall a. Any a => TypeEnv -> Type a -> Poly u) -> Poly u
typecheck_polytype te (MonoPTE monotype) cont =
  case typecheck_monotype te monotype of
    Mono a -> cont te a
typecheck_polytype te (ForallPTE var inner) cont = ForallP (hash var) (
    ExistsPoly $ typecheck_polytype
      (update_typeenv te var $ Mono (any_type :: Type a)) inner cont
    :: forall a. Any a => ExistsPoly u a)

lookup_var :: forall e. Env e -> String -> Poly (Ast e)
lookup_var NilEN var = MonoP $ Mono
  (ErrorA $ "unknown variable: '" ++ var ++ "'" :: Ast e Void)
lookup_var (ConsEN name ty rest) var =
  if var == name then
    MonoP $ Mono VarA
  else
    polymap (MonoP . Mono . LiftA) $ lookup_var rest var
lookup_var (LetEN name val rest) var =
  if var == name then
    val
    -- FIXME: here should be generating fresh ids for quantifiers inside val
    -- so that stuff like "let id = \x -> x in id id" or
    -- "let apply = \a f -> f a in apply apply (\app -> app 3 unary_minus)"
    -- should work
  else
    lookup_var rest var

-- TODO: add possibility of referencing type variables declared in outer scope!
-- so typecheck will take the same (String -> Mono Type) = TypeEnv parameter
-- as in typecheck_polytype' and typecheck_monotype

typecheck :: forall e. Typeable e => TypeEnv -> Env e -> Expr -> Poly (Ast e)
typecheck te e AddE = MonoP $ Mono $ AddA
typecheck te e (LiteralE val) = MonoP $ Mono $ LiteralA val
typecheck te e (AppE fun arg) =
  unify type_of_arg (typecheck te e fun) (Mono . type_of) (typecheck te e arg) cont
  where
    type_of_arg :: Any a => Ast e a -> Mono Type
    type_of_arg fun = case type_of fun of
      a `ArrowTT` b -> Mono a
    cont :: (Any a, Any b) => Ast e a -> Ast e b -> Poly (Ast e)
    cont fun arg = case type_of fun of
      a `ArrowTT` b -> case cast arg of
        Just correct_arg -> MonoP $ Mono $ fun `AppA` correct_arg
        Nothing -> MonoP $ Mono $ (ErrorA "wrong type of function argument" :: Ast e Void)
      _ -> MonoP $ Mono $ (ErrorA "_ is not a function" :: Ast e Void)
typecheck te e (LambdaE var_name ty body) = (typecheck_polytype te ty helper :: Poly (Ast e)) where
  helper :: forall a. Any a => TypeEnv -> Type a -> Poly (Ast e)
  helper te' tt = polymap (MonoP . Mono . (
      LambdaA :: forall b. Any b => Ast (Cons a e) b -> Ast e (a -> b)
    )) body_ast
    where
      body_ast :: Poly (Ast (Cons a e))
      body_ast = typecheck te' (ConsEN var_name tt e) body
typecheck te e (VarE name) = lookup_var e name

expr_1 =
  LambdaE "a" (ForallPTE "a" $ MonoPTE $ VarMTE "a") $
  LambdaE "f" (ForallPTE "b" $ MonoPTE $ VarMTE "a" `ArrowMTE` VarMTE "b") $
  VarE "f" `AppE` VarE "a"
ast_1 = typecheck (TypeEnv []) NilEN expr_1
type_1 = polymap (MonoP . Mono . type_of) ast_1

expr_2 = expr_1 `AppE` (LiteralE 5) `AppE` (AddE `AppE` (LiteralE 3))
ast_2 = typecheck (TypeEnv []) NilEN expr_2
type_2 = polymap (MonoP . Mono . type_of) ast_2

forcetype :: Any a => Poly (Ast Nil) -> Ast Nil a
forcetype (ForallP _ exists_poly) =
  case exists_poly of
    (ExistsPoly poly :: ExistsPoly (Ast Nil) TypeHole) -> forcetype poly
forcetype (MonoP (Mono ast)) = case cast ast of
  Just x -> x
  Nothing -> ErrorA $ "wrong type of: " ++ show ast

eval_poly :: Any a => Poly (Ast Nil) -> a
eval_poly = eval NilS . forcetype

main = do
  print $ type_1
  print $ ast_1
  print $ type_2
  print $ ast_2
  print $ (eval_poly (ast_2) :: Int)
