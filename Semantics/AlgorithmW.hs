{-# LANGUAGE CPP #-}
#include "../settings.hs"

module Semantics.AlgorithmW (run_algorithm_w) where

import Control.Monad.Trans.State
import Data.List (nub, (\\))

import Base hiding (Env)

--import Debug.Trace (trace)

run_algorithm_w :: ExtModuleTypeEnv -> UAst -> UAst
run_algorithm_w env a = fst $ runTCM env $ infer a

type Env = [(String, UPolyType)]

data TCState = TCState {
    env :: [(String, UPolyType)],
    stack :: [UMonoType],
    last_var :: Int,
    module_env :: ExtModuleTypeEnv
  }

type TCM = State TCState

free_vars_type :: UMonoType -> [String]
free_vars_type (UMonoType _ tt) = case tt of
  ArrowUMT a b -> nub $ free_vars_type a ++ free_vars_type b
  IntUMT -> []
  VarUMT s -> [s]
  HasFieldUMT (_, a) b -> nub $ free_vars_type a ++ free_vars_type b
  RecordConsUMT (_, a) b -> nub $ free_vars_type a ++ free_vars_type b
  RecordNilUMT -> []
  BoolUMT -> []
  MaybeUMT a -> free_vars_type a
  EitherUMT a b -> nub $ free_vars_type a ++ free_vars_type b
  CharUMT -> []
  ListUMT a -> free_vars_type a
  IO_UMT a -> free_vars_type a
  DynamicUMT -> []
  UnitUMT -> []
  PairUMT a b -> nub $ free_vars_type a ++ free_vars_type b
  TripleUMT a b c -> nub $ free_vars_type a ++ free_vars_type b ++ free_vars_type c

free_vars_scheme :: UPolyType -> [String]
free_vars_scheme (UPolyType vars tt) =
  free_vars_type tt \\ vars

free_vars_env :: Env -> [String]
free_vars_env env = nub $ helper env where
  helper [] = []
  helper ((_, scheme):t) = free_vars_scheme scheme ++ helper t

initTCState module_env = TCState {
    env = [], stack = [], last_var = 0, module_env = module_env
  }

runTCM :: ExtModuleTypeEnv -> TCM a -> a
runTCM module_env = flip evalState $ initTCState module_env

new_var :: SourceInfo -> String -> TCM UMonoType
new_var src prefix = do
  s <- get
  let v = last_var s + 1
  put $ s {last_var = v}
  return $ UMonoType src $ VarUMT $ prefix ++ show v

substitute_type :: String -> UMonoType -> UMonoType -> UMonoType
substitute_type name ty@(UMonoType _ ty') (UMonoType src tt) = UMonoType src $ case tt of
  VarUMT s -> if s == name then ty' else VarUMT s
  IntUMT -> IntUMT
  ArrowUMT a b -> ArrowUMT (substitute_type name ty a) (substitute_type name ty b)
  HasFieldUMT (f, a) b -> HasFieldUMT (f, substitute_type name ty a) (substitute_type name ty b)
  RecordConsUMT (f, a) b -> RecordConsUMT (f, substitute_type name ty a) (substitute_type name ty b)
  RecordNilUMT -> RecordNilUMT
  BoolUMT -> BoolUMT
  MaybeUMT a -> MaybeUMT $ substitute_type name ty a
  EitherUMT a b -> EitherUMT (substitute_type name ty a) (substitute_type name ty b)
  CharUMT -> CharUMT
  ListUMT a -> ListUMT $ substitute_type name ty a
  IO_UMT a -> IO_UMT $ substitute_type name ty a
  DynamicUMT -> DynamicUMT
  UnitUMT -> UnitUMT
  PairUMT a b -> PairUMT (substitute_type name ty a) (substitute_type name ty b)
  TripleUMT a b c -> TripleUMT (substitute_type name ty a) (substitute_type name ty b) (substitute_type name ty c)

substitute_scheme :: String -> UMonoType -> UPolyType -> UPolyType
substitute_scheme name ty (UPolyType vars tt) = UPolyType vars $
  if name `elem` vars then tt else substitute_type name ty tt

substitute_env :: String -> UMonoType -> Env -> Env
substitute_env name ty [] = []
substitute_env name ty ((var, h):t) = (var, substitute_scheme name ty h) : substitute_env name ty t

substitute :: String -> UMonoType -> TCM ()
substitute name ty = do
  s <- get
  put $ s {
      env = substitute_env name ty $ env s,
      stack = map (substitute_type name ty) $ stack s
    }

push_binding :: (String, UPolyType) -> TCM ()
push_binding (name, scheme) = do
  s <- get
  put $ s {env = (name, scheme):env s}

pop_binding :: TCM (String, UPolyType)
pop_binding = do
  s <- get
  case env s of
    h:t -> do
      put $ s {env = t}
      return h
    _ -> error "unreachable"

push_type :: UMonoType -> TCM ()
push_type tt = do
  s <- get
  put $ s {stack = tt : stack s}

pop_type :: TCM UMonoType
pop_type = do
  s <- get
  case stack s of
    h:t -> do
      put $ s {stack = t}
      return h
    _ -> error "unreachable"

lookup_env :: String -> Env -> UPolyType
lookup_env a [] = error $ "Not found: " ++ a
lookup_env a ((name, scheme):t) = if a == name then scheme else lookup_env a t

lookup_binding :: String -> TCM UPolyType
lookup_binding a = do
  s <- get
  return $ lookup_env a $ env s

lookup_module :: String -> TCM [(String, UPolyType)]
lookup_module name = let
    helper :: [(String, [(String, UPolyType)])] -> [(String, UPolyType)]
    helper [] = error "Unreachable code"
    helper ((n, m):t) = if n == name then m else helper t
  in do
    s <- get
    let ExtModuleTypeEnv e = module_env s
    return $ helper e

instantiate :: UPolyType -> TCM UMonoType
instantiate (UPolyType vars t@(UMonoType src tt)) = do
  s <- get
  nvars <- mapM (\v -> new_var src v) vars
  --trace ("<<" ++ show vars ++ ", " ++ show nvars ++ ">>") $
  return $ foldl (\tt (a, b) -> substitute_type a b tt) t $ zip vars nvars

generalize :: UMonoType -> TCM UPolyType
generalize tt = do
  s <- get
  let vars = free_vars_type tt \\ (free_vars_env $ env s)
  --trace ("<" ++ show vars ++ ">") $
  return $ UPolyType vars tt

unify_field_mandatory :: (String, UMonoType) -> UMonoType -> TCM ()
unify_field_mandatory (f, a) (UMonoType src b) = case b of
  HasFieldUMT (f', a') (UMonoType _ b') ->
    if f == f' then
      unify a a'
    else
      unify_field_optional (f, a) (UMonoType src b')
  RecordConsUMT (f', a') (UMonoType _ b') -> do
    if f == f' then
      unify a a'
    else
      unify_field_optional (f, a) (UMonoType src b')
  _ -> error $ "Undefined field '" ++ f ++ "'\n\n" ++ show_source src

unify_is_subset :: UMonoType -> UMonoType -> TCM ()
unify_is_subset (UMonoType src (RecordConsUMT (f, a) b)) rr@(UMonoType src' r) = do
  unify_field_mandatory (f, a) rr
  unify_is_subset b rr
unify_is_subset (UMonoType src RecordNilUMT) (UMonoType src' r) = return ()
unify_is_subset _ _ = error "unreachable"

unify_field_optional :: (String, UMonoType) -> UMonoType -> TCM ()
unify_field_optional (f, a) (UMonoType src b) = case b of
  HasFieldUMT (f', a') (UMonoType _ b') ->
    if f == f' then
      unify a a'
    else
      unify_field_optional (f, a) (UMonoType src b')
  RecordConsUMT (f', a') (UMonoType _ b') -> do
    if f == f' then
      unify a a'
    else
      unify_field_optional (f, a) (UMonoType src b')
  VarUMT _ -> return ()
  _ -> error $ "Undefined field '" ++ f ++ "'\n\n" ++ show_source src

unify arg@(UMonoType src arg_) arg'@(UMonoType src' arg_') = case (arg_, arg_') of
  (VarUMT u, t) -> bind_var u arg'
  (t, VarUMT u) -> bind_var u arg

  (HasFieldUMT (f, a) b, b') -> do
    unify_field_optional (f, a) arg'
    unify b (UMonoType src' b')
  (b', HasFieldUMT (f, a) b) -> do
    unify_field_optional (f, a) arg
    unify b arg

  (r@(RecordConsUMT (f, a) b), r'@(RecordConsUMT (f', a') b')) -> do
    unify_is_subset arg arg'
    unify_is_subset arg' arg

  (RecordNilUMT, RecordNilUMT) -> return ()

  (ArrowUMT a b, ArrowUMT a' b') -> do
    push_type b
    push_type b'
    unify a a'
    b' <- pop_type
    b <- pop_type
    unify b b'
  (IntUMT, IntUMT) -> return ()
  (BoolUMT, BoolUMT) -> return ()
  (MaybeUMT a, MaybeUMT a') -> unify a a'
  (EitherUMT a b, EitherUMT a' b') -> do
    push_type b
    push_type b'
    unify a a'
    b' <- pop_type
    b <- pop_type
    unify b b'
  (CharUMT, CharUMT) -> return ()
  (ListUMT a, ListUMT a') -> unify a a'
  (IO_UMT a, IO_UMT a') -> unify a a'
  (DynamicUMT, DynamicUMT) -> return ()
  (UnitUMT, UnitUMT) -> return ()
  (PairUMT a b, PairUMT a' b') -> do
    push_type b
    push_type b'
    unify a a'
    b' <- pop_type
    b <- pop_type
    unify b b'
  (TripleUMT a b c, TripleUMT a' b' c') -> do
    push_type c
    push_type c'
    push_type b
    push_type b'
    unify a a'
    b' <- pop_type
    b <- pop_type
    unify b b'
    c' <- pop_type
    c <- pop_type
    unify c c'
  (a, b) -> error $ "Types do not unify\n\n"
    ++ show arg ++ "\n vs. \n" ++ show arg'

bind_var :: String -> UMonoType -> TCM ()
bind_var u tt@(UMonoType src t) =
    if t == VarUMT u then
      return ()
    else if u `elem` free_vars_type tt then
      error $ "occur check fails: " ++ u ++ " vs. " ++ show tt ++ "\n\n" ++ show_source src
    else
      substitute u tt

infer :: UAst -> TCM (UAst, UMonoType)
infer (UAst src ast) = (>>= (\(a, tt) -> return (UAst src a, tt))) $ case ast of
  LiteralUA x -> return $ (LiteralUA x, UMonoType src $ IntUMT)
  StringUA x -> return $ (StringUA x, UMonoType src $ ListUMT $ UMonoType src $ CharUMT)
  VarUA n -> do
    scheme <- lookup_binding n
    (UMonoType _ ty) <- instantiate scheme
    return (VarUA n, UMonoType src ty)
  LambdaUA (n, arg_t) e -> do
    arg_type <- instantiate arg_t
    push_type arg_type
    push_binding (n, UPolyType [] arg_type)
    (e', result_type) <- infer e
    arg_type <- pop_type
    pop_binding
    arg_type_gen <- generalize arg_type
    return (LambdaUA (n, arg_type_gen) e', UMonoType src $ ArrowUMT arg_type result_type)
  AppUA func arg -> do
    result_type <- new_var src "tmp_"
    push_type result_type
    (func', func_type) <- infer func
    push_type func_type
    (arg', arg_type) <- infer arg
    func_type <- pop_type
    push_type func_type
    push_type arg_type
    unify func_type $ UMonoType src $ ArrowUMT arg_type result_type
    arg_type <- pop_type
    func_type <- pop_type
    result_type <- pop_type
    --return $ trace ("| " ++ show func_type ++ " = " ++ show arg_type ++ " ~> " ++ show result_type__ ++ " |") $
    return (AppUA func' arg', result_type)
  LetUA (n, a) b -> do
    (a', type_a) <- infer a
    scheme_a <- generalize type_a
    push_binding (n, scheme_a)
    (b', type_b) <- infer b
    pop_binding
    return (LetUA (n, a') b', type_b)
  RecordNilUA -> return $ (RecordNilUA, UMonoType src $ RecordNilUMT)
  OpenUA m e -> do
    mod_ <- lookup_module m
    mapM_ push_binding mod_
    (e', type_e) <- infer e
    mapM_ (const pop_binding) mod_
    return (OpenUA m e', type_e)
