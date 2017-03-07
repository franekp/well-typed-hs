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

import Types
import Instances
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
  NilE :: Env Nil
  ConsE :: (Any h, Typeable t) => String -> Type h -> Env t -> Env (Cons h t)

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
  | LambdaE String TypeExpr Expr
  | VarE String

data TypeExpr = IntTE | ArrowTE TypeExpr TypeExpr

typecheck_type :: TypeExpr -> Mono Type
typecheck_type IntTE = Mono IntTT
typecheck_type (a `ArrowTE` b) = case (typecheck_type a, typecheck_type b) of
  (Mono a', Mono b') -> Mono $ a' `ArrowTT` b'

lookup_var :: forall e. Env e -> String -> Mono (Ast e)
lookup_var NilE var = Mono (ErrorA $ "unknown variable: '" ++ var ++ "'" :: Ast e Void)
lookup_var (ConsE name ty rest) var =
  if var == name then Mono VarA else
    case lookup_var rest var of
      Mono res -> Mono $ LiftA res

typecheck :: forall e. Typeable e => Env e -> Expr -> Poly (Ast e)
typecheck e AddE = MonoP $ Mono $ AddA
typecheck e (LiteralE val) = MonoP $ Mono $ LiteralA val
typecheck e (AppE fun arg) =
  unify type_of_arg (typecheck e fun) (Mono . type_of) (typecheck e arg) cont
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
typecheck e (LambdaE var_name ty body) =
  case typecheck_type ty of
    Mono tt ->
      let body_ast = typecheck (ConsE var_name tt e) body in
       polymap (\b -> MonoP $ Mono $ LambdaA b) body_ast
typecheck e (VarE name) = MonoP $ lookup_var e name
