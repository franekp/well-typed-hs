{- LANGUAGE GADTs, OverlappingInstances, IncoherentInstances, TypeFamilies, DeriveFunctor, ViewPatterns, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, UndecidableInstances #-}
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

import Data.Void
import Data.Typeable
import Data.Dynamic

data UntypedAst = AddU | MultU | NegateU
  | LiteralU Int
  | AppU UntypedAst UntypedAst
  | LambdaU String UntypedType UntypedAst
  | VarU String
  | IdU | ComposeU | ApplyU

data UntypedType = IntUT | ArrowUT UntypedType UntypedType

data TypedType a where
  ArrowTT :: TypedType a -> TypedType b -> TypedType (a -> b)
  IntTT :: TypedType Int

deriving instance Show (TypedType a)

data OpaqueType where
  OpaqueType :: forall a. LangType a => TypedType a -> OpaqueType

type Store = (forall a. LangType a => String -> TypedType a -> Maybe a)

data Ast a where
  AddT :: Ast (Int -> Int -> Int)
  MultT :: Ast (Int -> Int -> Int)
  NegateT :: Ast (Int -> Int)
  LiteralT :: Int -> Ast Int
  ErrorT :: LangType a => String -> Ast a
  AppT :: (LangType a, LangType b) => Ast (a -> b) -> Ast a -> Ast b
  VarT :: LangType a => String -> TypedType a -> Ast a
  LambdaT :: (LangType a, LangType b) => String -> TypedType a -> Ast b -> Ast (a -> b)
  IdT :: LangType a => Ast (a -> a)
  ComposeT :: LangType a => Ast ((a -> a) -> (a -> a) -> (a -> a))
  ApplyT :: LangType a => Ast (a -> (a -> a) -> a)

deriving instance Typeable1 Ast
deriving instance Show (Ast a)

class (Typeable a) => LangType a where
  make_app_node_arity :: Ast a -> OpaqueAst -> OpaqueAst

instance LangType Int where
  make_app_node_arity fun arg = case fun of
    ErrorT a -> OpaqueAst (ErrorT a :: Ast Void)
    _ -> OpaqueAst (ErrorT "wrong arity" :: Ast Void)

instance LangType Void where
  make_app_node_arity fun arg = case fun of
    ErrorT a -> OpaqueAst (ErrorT a :: Ast Void)
    _ -> OpaqueAst (ErrorT "wrong arity" :: Ast Void)

instance (LangType a, LangType b) => LangType (a -> b) where
  make_app_node_arity fun arg = case forcetype arg of
    ErrorT msg -> OpaqueAst $ (ErrorT $ (msg ++ " --- expected type compatibile with: " ++ (show $ dynTypeRep $ toDyn fun) ++ "//  " ++ show fun) :: Ast Void)
    argg -> OpaqueAst $ AppT fun argg

data OpaqueAstPolyHelperExists a where
  OpaqueAstPolyHelperExists :: (LangType a, LangType b) => Ast b -> OpaqueAstPolyHelperExists a

data OpaqueAst where
  OpaqueAst :: LangType a => Ast a -> OpaqueAst
  OpaqueAstPoly :: (forall a. LangType a => OpaqueAstPolyHelperExists a) -> OpaqueAst

instance Show OpaqueAst where
  show (OpaqueAst a) = show a

typecheck_ast :: UntypedAst -> OpaqueAst
typecheck_type :: UntypedType -> OpaqueType
forcetype :: LangType a => OpaqueAst -> Ast a

typecheck_type IntUT = OpaqueType IntTT
typecheck_type (ArrowUT a b) =
  case typecheck_type a of
    OpaqueType at -> case typecheck_type b of
      OpaqueType bt -> OpaqueType $ ArrowTT at bt

typecheck_ast a = typecheck_inner (\_ -> Nothing) a where
  make_app_node :: OpaqueAst -> OpaqueAst -> OpaqueAst
  make_app_node (OpaqueAst func) a = make_app_node_arity func a
  make_app_node (OpaqueAstPoly polyhelperexists) (OpaqueAst arg) =
    let
      do_stuff :: (LangType a) => OpaqueAstPolyHelperExists a -> Ast a -> OpaqueAst
      do_stuff phe a = case phe of
        OpaqueAstPolyHelperExists func ->
          make_app_node_arity func (OpaqueAst a)
    in do_stuff polyhelperexists arg

  typecheck_inner :: (String -> Maybe UntypedType) -> UntypedAst -> OpaqueAst
  typecheck_inner ctx AddU = OpaqueAst $ AddT
  typecheck_inner ctx MultU = OpaqueAst $ MultT
  typecheck_inner ctx NegateU = OpaqueAst $ NegateT
  typecheck_inner ctx (LiteralU n) = OpaqueAst $ LiteralT n
  typecheck_inner ctx (AppU func a) =
    make_app_node (typecheck_inner ctx func) (typecheck_inner ctx a)
  typecheck_inner ctx (VarU name) = case ctx name of
    Nothing -> OpaqueAst $ (ErrorT ("undefined variable: " ++ name) :: Ast Void)
    Just ty -> case typecheck_type ty of
      OpaqueType tt -> OpaqueAst $ VarT name tt
  typecheck_inner ctx (LambdaU name ty body) =
    let newctx a = if a == name then Just ty else ctx a in
    case typecheck_inner newctx body of
      OpaqueAst bodynode -> case typecheck_type ty of
        OpaqueType tt -> OpaqueAst $ LambdaT name tt bodynode
  typecheck_inner ctx IdU =
    OpaqueAstPoly $
      (OpaqueAstPolyHelperExists :: LangType a => Ast (a -> a) -> OpaqueAstPolyHelperExists a)
      IdT
  typecheck_inner ctx ComposeU =
    OpaqueAstPoly $
      -- THIS IS WRONG! ; the last argument of OpaqueAstPolyHelperExists should be (a -> a), not a. But it does not compile otherwise (TODO)
      (OpaqueAstPolyHelperExists :: LangType a => Ast ((a -> a) -> (a -> a) -> (a -> a)) -> OpaqueAstPolyHelperExists a)
      ComposeT
  typecheck_inner ctx ApplyU =
    OpaqueAstPoly $
      (OpaqueAstPolyHelperExists :: LangType a => Ast (a -> (a -> a) -> a) -> OpaqueAstPolyHelperExists a)
      ApplyT

forcetype (OpaqueAst a) = case cast a of
  Just x -> x
  Nothing -> ErrorT $ "wrong type of: " ++ show a

eval :: LangType a => Store -> Ast a -> a
eval s AddT = (\a -> \b -> a + b)
eval s MultT = (\a -> \b -> a * b)
eval s NegateT = (\a -> -a)
eval s (LiteralT n) = n
eval s (AppT func a) = (eval s func) (eval s a)
eval s (VarT name tt) = let Just val = s name tt in val
eval s (LambdaT name tt body) = result where
  result param = eval new_s body where
    new_s :: forall a. LangType a => String -> TypedType a -> Maybe a
    new_s v t = case (cast param) of
      Just p ->
        if v == name && True
          then Just p  -- very important 'Just' here, it compiles without that too!
          else s v t
      Nothing -> s v t
eval s IdT = (\x -> x)
eval s ApplyT = (\x -> \f -> f x)

example1 =
  AppU (LambdaU "arg" (ArrowUT IntUT (ArrowUT IntUT IntUT)) (AppU (AppU (VarU "arg") (LiteralU 11)) (LiteralU 6))) AddU

example2 = AppU IdU (LiteralU 5)

example3 = AppU
  (AppU (AppU ComposeU
    (LambdaU "x" IntUT (AppU (AppU MultU (VarU "x")) (VarU "x") ) )
  ) (LambdaU "x" IntUT (AppU (AppU MultU (VarU "x")) (VarU "x") ) ) ) (LiteralU 3)

example4 = AppU
  (AppU ApplyU (LiteralU 5))
  (LambdaU "x" IntUT (AppU (AppU MultU (VarU "x")) (VarU "x") ))

main = do
  putStrLn $ show $ (typecheck_ast example4)
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example1) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example2) :: Ast Int )
  -- putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example3) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example4) :: Ast Int )
