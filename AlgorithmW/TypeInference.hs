import qualified Text.PrettyPrint as PP

import Control.Monad.Trans.State
import Data.List (nub, (\\))

--import Debug.Trace (trace)

trace = const id

data Exp     =  EVar String
             |  EApp Exp Exp
             |  EAbs String Exp
             |  ELet String Exp Exp
             |  EInt Integer
             |  EBool Bool
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TInt
             |  TBool
             |  TFun Type Type
             deriving (Eq, Ord)

data Scheme  =  Scheme [String] Type

type Env = [(String, Scheme)]

data TCState = TCState {
    env :: [(String, Scheme)],
    stack :: [Type],
    last_var :: Int
  }

type TCM = State TCState

free_vars_type :: Type -> [String]
free_vars_type TInt = []
free_vars_type TBool = []
free_vars_type (TFun a b) = nub $ free_vars_type a ++ free_vars_type b
free_vars_type (TVar s) = [s]

free_vars_scheme :: Scheme -> [String]
free_vars_scheme (Scheme vars tt) =
  free_vars_type tt \\ vars

free_vars_env :: Env -> [String]
free_vars_env env = nub $ helper env where
  helper [] = []
  helper ((_, scheme):t) = free_vars_scheme scheme ++ helper t

initTCState = TCState {env = [], stack = [], last_var = 0}

runTCM :: TCM a -> a
runTCM = flip evalState initTCState

new_var :: TCM Type
new_var = do
  s <- get
  let v = last_var s + 1
  put $ s {last_var = v}
  return $ TVar $ "a_" ++ show v

substitute_type :: String -> Type -> Type -> Type
substitute_type name ty (TVar s) = if s == name then ty else TVar s
substitute_type name ty TInt = TInt
substitute_type name ty TBool = TBool
substitute_type name ty (TFun a b) = TFun (substitute_type name ty a) (substitute_type name ty b)

substitute_scheme :: String -> Type -> Scheme -> Scheme
substitute_scheme name ty (Scheme vars tt) = Scheme vars $
  if name `elem` vars then tt else substitute_type name ty tt

substitute_env :: String -> Type -> Env -> Env
substitute_env name ty [] = []
substitute_env name ty ((var, h):t) = (var, substitute_scheme name ty h) : substitute_env name ty t

substitute :: String -> Type -> TCM ()
substitute name ty = do
  s <- get
  put $ s {
      env = substitute_env name ty $ env s,
      stack = map (substitute_type name ty) $ stack s
    }

push_binding :: (String, Scheme) -> TCM ()
push_binding (name, scheme) = do
  s <- get
  put $ s {env = (name, scheme):env s}

pop_binding :: TCM (String, Scheme)
pop_binding = do
  s <- get
  let h:t = env s
  put $ s {env = t}
  return h

push_type :: Type -> TCM ()
push_type tt = do
  s <- get
  put $ s {stack = tt : stack s}

pop_type :: TCM Type
pop_type = do
  s <- get
  let h:t = stack s
  put $ s {stack = t}
  return h

lookup_env :: String -> Env -> Scheme
lookup_env a [] = error $ "Not found: " ++ a
lookup_env a ((name, scheme):t) = if a == name then scheme else lookup_env a t

lookup_binding :: String -> TCM Scheme
lookup_binding a = do
  s <- get
  return $ lookup_env a $ env s

instantiate :: Scheme -> TCM Type
instantiate (Scheme vars t) = do
  s <- get
  nvars <- mapM (\_ -> new_var) vars
  trace ("<<" ++ show vars ++ ", " ++ show nvars ++ ">>") $ return $ foldl (\tt (a, b) -> substitute_type a b tt) t $ zip vars nvars

generalize :: Type -> TCM Scheme
generalize tt = do
  s <- get
  let vars = free_vars_type tt \\ (free_vars_env $ env s)
  trace ("<" ++ show vars ++ ">") $ return $ Scheme vars tt

unify :: Type -> Type -> TCM ()
unify TInt TInt = return ()
unify TBool TBool = return ()
unify (a `TFun` b) (a' `TFun` b') = do
  push_type b
  push_type b'
  unify a a'
  b' <- pop_type
  b <- pop_type
  unify b b'
unify (TVar u) t = bind_var u t
unify t (TVar u) = bind_var u t
unify a b = error $ "types do not unify: "
  ++ show a ++ " vs. " ++ show b

bind_var :: String -> Type -> TCM ()
bind_var u t =
    if t == TVar u then
      return ()
    else if u `elem` free_vars_type t then
      error $ "occur check fails: " ++ u ++ " vs. " ++ show t
    else
      substitute u t

infer :: Exp -> TCM Type
infer (EInt _) = return TInt
infer (EBool _) = return TBool
infer (EVar n) = do
  scheme <- lookup_binding n
  ty <- instantiate scheme
  return ty
infer (EAbs n e) = do
  arg_type <- new_var
  push_type arg_type
  push_binding (n, Scheme [] arg_type)
  result_type <- infer e
  arg_type <- pop_type
  pop_binding
  return $ TFun arg_type result_type
infer (EApp func arg) = do
  result_type <- new_var
  push_type result_type
  infer func >>= push_type
  infer arg >>= push_type
  arg_type <- pop_type
  func_type <- pop_type
  push_type func_type
  push_type arg_type
  unify func_type $ TFun arg_type result_type
  arg_type <- pop_type
  func_type <- pop_type
  result_type__ <- pop_type
  return $ trace ("| " ++ show func_type ++ " = " ++ show arg_type ++ " ~> " ++ show result_type__ ++ " |") $ result_type__
infer (ELet n a b) = do
  scheme_a <- (infer a >>= generalize)
  push_binding (n, scheme_a)
  type_b <- infer b
  pop_binding
  return type_b


e0  =  ELet "id" (EAbs "x" (EVar "x"))
        (EVar "id")

e1  =  ELet "id" (EAbs "x" (EVar "x"))
        (EApp (EVar "id") (EVar "id"))

e2  =  ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
        (EApp (EVar "id") (EVar "id"))

e3  =  ELet "id" (EAbs "x" (ELet "y" (EVar "x") (EVar "y")))
        (EApp (EApp (EVar "id") (EVar "id")) (EInt 2))

e4  =  ELet "id" (EAbs "x" (EApp (EVar "x") (EVar "x")))
        (EVar "id")

e5  =  EAbs "m" (ELet "y" (EVar "m")
                 (ELet "x" (EApp (EVar "y") (EBool True))
                       (EVar "x")))

test :: Exp -> IO ()
test e = do
  let t = runTCM $ infer e
  putStrLn $ show e ++ "\n :: " ++ show t
  putStrLn $ "\n------\n"

main :: IO ()
main = mapM_ test [e0, e1, e2, e3, e5]

instance Show Type where
    showsPrec _ x = shows (prType x)

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType TInt        =   PP.text "Int"
prType TBool       =   PP.text "Bool"
prType (TFun t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show Exp where
    showsPrec _ x = shows (prExp x)

prExp                  ::  Exp -> PP.Doc
prExp (EVar name)      =   PP.text name
prExp (EInt i)         =   PP.integer i
prExp (EBool b)        =   if b then PP.text "True" else PP.text "False"
prExp (ELet x b body)  =   PP.text "let" PP.<+>
                           PP.text x PP.<+> PP.text "=" PP.<+>
                           prExp b PP.<+> PP.text "in" PP.$$
                           PP.nest 2 (prExp body)
prExp (EApp e1 e2)     =   prExp e1 PP.<+> prParenExp e2
prExp (EAbs n e)       =   PP.char '\\' PP.<+> PP.text n PP.<+>
                           PP.text "->" PP.<+>
                           prExp e

prParenExp    ::  Exp -> PP.Doc
prParenExp t  =   case t of
                    ELet _ _ _  -> PP.parens (prExp t)
                    EApp _ _    -> PP.parens (prExp t)
                    EAbs _ _    -> PP.parens (prExp t)
                    _           -> prExp t

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> PP.Doc
prScheme (Scheme vars t)  =   PP.text "All" PP.<+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<> PP.text "." PP.<+> prType t
