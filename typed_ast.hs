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

data UntypedAst = AddU | MultU | NegateU
  | LiteralU Int
  | AppU UntypedAst UntypedAst
  | LambdaU String UntypedType UntypedAst
  | VarU String
  | IdU | ComposeU | ApplyU | Choose2U

data UntypedType = IntUT | ArrowUT UntypedType UntypedType

data TypedType a where
  ArrowTT :: (LangType a, LangType b) => TypedType a -> TypedType b -> TypedType (a -> b)
  IntTT :: TypedType Int
  VoidTT :: TypedType Void

data TypeLocation = HereTL | LeftTL TypeLocation | RightTL TypeLocation | BothTL TypeLocation TypeLocation

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
  ApplyT :: (LangType a, LangType b) => Ast (a -> (a -> b) -> b)
  Choose2T :: LangType a => Ast (Int -> a -> a -> a)

deriving instance Typeable1 Ast
deriving instance Show (Ast a)
deriving instance Show TypeLocation

class (Typeable a) => LangType a where
  get_type :: LangType a => TypedType a

instance LangType Int where
  get_type = IntTT

instance LangType Void where
  get_type = VoidTT

instance (LangType a, LangType b) => LangType (a -> b) where
  get_type = ArrowTT (get_type :: TypedType a) (get_type :: TypedType b)

type_of :: LangType a => Ast a -> TypedType a
type_of a = get_type

make_app_mono_mono :: (LangType a, LangType b) => Ast a -> Ast b -> OpaqueAst
make_app_mono_mono fun arg = case type_of fun of
  IntTT -> case fun of
    ErrorT a -> OpaqueAst (ErrorT a :: Ast Void)
    _ -> OpaqueAst (ErrorT "Int is not a function!" :: Ast Void)
  VoidTT -> case fun of
    ErrorT a -> OpaqueAst (ErrorT a :: Ast Void)
    _ -> OpaqueAst (ErrorT "Void is not a function!" :: Ast Void)
  ArrowTT a b -> case cast arg of
    Just argg -> OpaqueAst $ AppT fun argg
    Nothing -> OpaqueAst $ (ErrorT $
      ("Type mismatch: trying to apply " ++ (show $ type_of fun) ++ " to "
      ++ (show $ type_of arg)) :: Ast Void)

polyast_error :: String -> PolyAst Void
polyast_error a = PolyAst $ OpaqueAst $ (ErrorT a :: Ast Void)

unify :: forall res a. LangType a =>
  TypeLocation -> (forall x. LangType x => PolyAst x) -> TypedType a
  -> (forall y. LangType y => PolyAst y -> res) -> res
unify HereTL phe tt cont = make_result phe tt cont where
  make_result :: LangType x => PolyAst x -> TypedType x -> (forall y. LangType y => PolyAst y -> res) -> res
  make_result phe_ tt_ cont_ = cont_ phe_
unify (LeftTL _) phe IntTT cont = cont $ polyast_error "expected function, got IntTT"
unify (RightTL _) phe IntTT cont = cont $ polyast_error "expected function, got IntTT"
unify (BothTL _ _) phe IntTT cont = cont $ polyast_error "expected function, got IntTT"
unify (LeftTL left) phe (ArrowTT l r) cont = unify left phe l cont
unify (RightTL right) phe (ArrowTT l r) cont = unify right phe r cont
unify (BothTL right left) phe (ArrowTT l r) cont = unify left phe l cont  -- ???

make_app_poly_mono :: LangType a => TypeLocation -> (forall x. LangType x => PolyAst x) -> Ast a -> OpaqueAst
make_app_poly_mono (LeftTL left) phe arg = unify left phe (type_of arg) do_stuff where
  do_stuff :: PolyAst x -> OpaqueAst
  do_stuff (PolyAst (OpaqueAst fun)) = make_app_mono_mono fun arg
  do_stuff (PolyAst (OpaqueAstPoly loc newphe)) = make_app_poly_mono loc newphe arg
make_app_poly_mono (BothTL left right) phe arg = unify left phe (type_of arg) do_stuff where
  do_stuff :: PolyAst x -> OpaqueAst
  do_stuff (PolyAst (OpaqueAst fun)) = make_app_mono_mono fun arg
  do_stuff (PolyAst (OpaqueAstPoly loc newphe)) = make_app_poly_mono loc newphe arg
make_app_poly_mono (RightTL right) phe arg = OpaqueAstPoly right $ do_stuff phe arg where
  -- this is the true type for that, but it did not worked - this more specific type is automagically generalized somehow
  -- and conveys information that each x from the left corresponds to x from the right - the type variable stays the same
  -- do_stuff :: LangType aa => (forall x. LangType x => PolyAst x) -> Ast aa -> (forall y. LangType y => PolyAst y)
  do_stuff :: (LangType aa, LangType x) => PolyAst x -> Ast aa -> PolyAst x
  do_stuff (PolyAst (OpaqueAst fun)) arg =
    PolyAst $ make_app_mono_mono fun arg
  do_stuff (PolyAst (OpaqueAstPoly loc newphe)) arg =
    PolyAst $ make_app_poly_mono loc newphe arg
make_app_poly_mono HereTL phe arg = OpaqueAst $ (ErrorT $ "Trying to use variable of generic type as a function with argument: " ++ show arg :: Ast Void)

make_app :: OpaqueAst -> OpaqueAst -> OpaqueAst
make_app (OpaqueAst func) (OpaqueAst a) = make_app_mono_mono func a
make_app (OpaqueAstPoly loc polyhelperexists) (OpaqueAst arg) =
 make_app_poly_mono loc polyhelperexists arg

data PolyAst x where
  PolyAst :: LangType x => OpaqueAst -> PolyAst x

data OpaqueAst where
  OpaqueAst :: LangType a => Ast a -> OpaqueAst
  OpaqueAstPoly :: TypeLocation -> (forall x. LangType x => PolyAst x) -> OpaqueAst

instance Show OpaqueAst where
  show (OpaqueAst a) = show a
  show (OpaqueAstPoly tl polyast) = case (polyast :: PolyAst Void) of
    PolyAst opaque_ast -> "[" ++ show tl ++ "] " ++ show opaque_ast

typecheck_ast :: UntypedAst -> OpaqueAst
typecheck_type :: UntypedType -> OpaqueType
forcetype :: LangType a => OpaqueAst -> Ast a

typecheck_type IntUT = OpaqueType IntTT
typecheck_type (ArrowUT a b) =
  case typecheck_type a of
    OpaqueType at -> case typecheck_type b of
      OpaqueType bt -> OpaqueType $ ArrowTT at bt

typecheck_ast a = typecheck_inner (\_ -> Nothing) a where
  typecheck_inner :: (String -> Maybe UntypedType) -> UntypedAst -> OpaqueAst
  typecheck_inner ctx AddU = OpaqueAst $ AddT
  typecheck_inner ctx MultU = OpaqueAst $ MultT
  typecheck_inner ctx NegateU = OpaqueAst $ NegateT
  typecheck_inner ctx (LiteralU n) = OpaqueAst $ LiteralT n
  typecheck_inner ctx (AppU func a) =
    make_app (typecheck_inner ctx func) (typecheck_inner ctx a)
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
    OpaqueAstPoly (BothTL HereTL HereTL) $
      ((PolyAst . OpaqueAst) :: LangType a => Ast (a -> a) -> PolyAst a)
      IdT
  typecheck_inner ctx ComposeU =
    -- should be (BothTL (BothTL HereTL HereTL) (BothTL (BothTL HereTL HereTL) (BothTL HereTL HereTL))) but not used for now
    OpaqueAstPoly (LeftTL $ BothTL HereTL HereTL) $
      ((PolyAst . OpaqueAst) :: LangType a => Ast ((a -> a) -> (a -> a) -> (a -> a)) -> PolyAst a)
      ComposeT
  typecheck_inner ctx ApplyU = OpaqueAstPoly (LeftTL HereTL) (helper ApplyT)
    where
      helper :: forall a. LangType a => (forall b. LangType b => Ast (a -> (a -> b) -> b)) -> PolyAst a
      helper arg =
        let
          inner :: LangType a => (forall b. LangType b => PolyAst b) -> PolyAst a
          inner polyast = PolyAst $ OpaqueAstPoly (RightTL $ LeftTL $ RightTL HereTL) polyast
        in inner ( (PolyAst . OpaqueAst :: LangType b => Ast (a -> (a -> b) -> b) -> PolyAst b) arg)
  typecheck_inner ctx Choose2U =
    OpaqueAstPoly (RightTL $ LeftTL HereTL) $
      ((PolyAst . OpaqueAst) :: LangType a => Ast (Int -> a -> a -> a) -> PolyAst a)
      Choose2T

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
        if v == name
          then Just p  -- very important 'Just' here, it compiles without that too!
          else s v t
      Nothing -> s v t
eval s IdT = (\x -> x)
eval s ComposeT = (\f -> \g -> \x -> f (g x))
eval s ApplyT = (\x -> \f -> f x)
eval s Choose2T = (\x -> \a -> \b -> if x == 0 then a else b)

example1 =
  AppU (LambdaU "arg" (ArrowUT IntUT (ArrowUT IntUT IntUT)) (AppU (AppU (VarU "arg") (LiteralU 11)) (LiteralU 6))) AddU

example2 = AppU IdU (LiteralU 5)

example3 = AppU
  (AppU (AppU ComposeU
    (LambdaU "x" IntUT (AppU (AppU MultU (VarU "x")) (VarU "x") ) )
  ) (LambdaU "x" IntUT (AppU (AppU MultU (VarU "x")) (VarU "x") ) ) ) (LiteralU 3)

example4 =
  ApplyU `AppU` (LambdaU "x" IntUT $ MultU `AppU` VarU "x" `AppU` VarU "x") `AppU`
  (LambdaU "f" (ArrowUT IntUT IntUT) $ VarU "f" `AppU` LiteralU 7)

example5 = (Choose2U `AppU` (LiteralU 0) `AppU` AddU `AppU` MultU) `AppU` (LiteralU 1) `AppU` (LiteralU 2)

main = do
  putStrLn $ show $ (typecheck_ast example4)
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example1) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example2) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example3) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example4) :: Ast Int )
  putStrLn $ show $ eval (\_ -> \_ -> Nothing) (forcetype (typecheck_ast example5) :: Ast Int )
