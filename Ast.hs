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

typecheck_monotype :: (String -> Mono Type) -> MonoTypeExpr -> Mono Type
typecheck_monotype ctx IntMTE = Mono IntTT
typecheck_monotype ctx (a `ArrowMTE` b) =
  case (typecheck_monotype ctx a, typecheck_monotype ctx b) of
    (Mono a', Mono b') -> Mono $ a' `ArrowTT` b'
typecheck_monotype ctx (VarMTE var) = ctx var

hash :: String -> Int
hash = foldl' (\h c -> 33*h `xor` fromEnum c) 5381

-- FIXME: this should be a monad generating unique ids, not hashes!
typecheck_polytype' :: (String -> Mono Type) -> PolyTypeExpr -> Poly Type
typecheck_polytype' ctx (MonoPTE monotype) = MonoP $ typecheck_monotype ctx monotype
typecheck_polytype' ctx (ForallPTE var inner) = ForallP (hash var) (
    ExistsPoly $ typecheck_polytype'
      (\x -> if x == var then Mono (any_type :: Type a) else ctx x) inner
    :: forall a. Any a => ExistsPoly Type a)

typecheck_polytype :: PolyTypeExpr -> Poly Type
typecheck_polytype = typecheck_polytype' (\_ -> error "not found")

lookup_var :: forall e. Env e -> String -> Mono (Ast e)
lookup_var NilE var = Mono (ErrorA $ "unknown variable: '" ++ var ++ "'" :: Ast e Void)
lookup_var (ConsE name ty rest) var =
  if var == name then Mono VarA else
    case lookup_var rest var of
      Mono res -> Mono $ LiftA res

-- TODO: add possibility of referencing type variables declared in outer scope!
-- so typecheck will take the same (String -> Mono Type) = TypeEnv parameter
-- as in typecheck_polytype' and typecheck_monotype

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
typecheck e (LambdaE var_name ty body) = polymap helper $ (typecheck_polytype ty :: Poly Type) where
  helper :: forall a. Any a => Type a -> Poly (Ast e)
  helper tt = polymap (MonoP . Mono . (
      LambdaA :: forall b. Any b => Ast (Cons a e) b -> Ast e (a -> b)
    )) body_ast
    where
      body_ast :: Poly (Ast (Cons a e))
      body_ast = typecheck (ConsE var_name tt e) body
typecheck e (VarE name) = MonoP $ lookup_var e name

expr_1 =
  LambdaE "a" (ForallPTE "a" $ MonoPTE $ VarMTE "a") $
  LambdaE "f" (ForallPTE "b" $ ForallPTE "c" $ MonoPTE $ VarMTE "b" `ArrowMTE` VarMTE "c") $
  VarE "f" `AppE` VarE "a"
ast_1 = typecheck NilE expr_1
type_1 = polymap (MonoP . Mono . type_of) ast_1

main = do
  print $ type_1
  print $ ast_1
