-- copied from: https://gist.github.com/worldsayshi/8853946
-----------------------------------------------------------------------------
-- | Example for loading Haskell source code dynamically using the GHC api
--
-- Useful links:
-- GHC api:
-- http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/GHC.html
-- Wiki:
-- http://www.haskell.org/haskellwiki/GHC/As_a_library
-----------------------------------------------------------------------------
module ExtLoader.Loader (load_ext_modules) where

import GHC
import GhcMonad (liftIO)
import Name (getOccString)
import DynFlags
import Data.Dynamic (fromDyn, fromDynamic)

import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))
import Control.Monad.IO.Class

import Base

load_ext_modules :: UAst -> IO ExtModuleEnv
load_ext_modules = load_ext_modules_by_name . dump_module_names

dump_module_names :: UAst -> [String]
dump_module_names (UAst src (LiteralUA _)) = []
dump_module_names (UAst src (StringUA _)) = []
dump_module_names (UAst src (AppUA f a)) = dump_module_names f ++ dump_module_names a
dump_module_names (UAst src (LambdaUA (_, _) a)) = dump_module_names a
dump_module_names (UAst src (VarUA _)) = []
dump_module_names (UAst src (LetUA (_, a) b)) = dump_module_names a ++ dump_module_names b
dump_module_names (UAst src (RecordNilUA)) = []
dump_module_names (UAst src (RecordConsUA (_, a) rest)) = dump_module_names a ++ dump_module_names rest
dump_module_names (UAst src (RecordGetUA _ a)) = dump_module_names a
dump_module_names (UAst src (OpenUA m a)) = (m:dump_module_names a)
dump_module_names (UAst src (TypeDefUA _ a)) = dump_module_names a

load_ext_modules_by_name :: [String] -> IO ExtModuleEnv
load_ext_modules_by_name modules = do
  script_path <- getExecutablePath
  let script_dir = takeDirectory script_path
  runGhc (Just GHC_LIBDIR) $ do
    --putString ":::"
    modSums <- initSession modules script_dir
    --putString ":::"
    imports <- mapM parseImportDecl ["import qualified " ++ m | m <- modules]
    setContext [IIDecl i | i <- imports]
    dynVals <- mapM dynCompileExpr [m ++ ".module_exports" | m <- modules]
    let res = [(fromDynamic d :: Maybe ExtModule) | d <- dynVals]
    let f (mod, a) = case a of {Just m -> m ; Nothing -> error $ "wrong type of " ++ mod ++ ".module_exports"}
    return $ ExtModuleEnv $ zip modules $ map f (zip modules res)

-- | Init interactive session and load modules
initSession modStrNames script_dir = do
  dflags <- getSessionDynFlags
  setSessionDynFlags
    $ flip xopt_set Opt_Cpp
    $ dflags {
      --hscTarget = HscInterpreted
      ghcLink   = LinkInMemory
      , importPaths = importPaths dflags ++ [script_dir]
      , includePaths = includePaths dflags ++ [script_dir </> "ExtLoader"]
      , settings = (settings dflags) {
          -- disable "-traditional" option because we want real CPP with real macros, not traditional
          sPgm_P = ("gcc", [Option "-E"])
        }
      }
  targets <- mapM
              (\modStrName -> do
                  --putString modStrName
                  target <- guessTarget ("*"++modStrName++".hs") Nothing
                  return target
              ) modStrNames
  setTargets targets
  load LoadAllTargets
  modSums <- mapM
              (\modStrName -> do
                  --putString modStrName
                  modSum <- getModSummary $ mkModuleName modStrName
                  return $ ms_mod modSum
              ) modStrNames
  return modSums

-- | List exported names of this or a sibling module
listExports mod = do
  maybeModInfo <- getModuleInfo mod
  case maybeModInfo of
    (Just modInfo) -> do
      let expNames = modInfoExports modInfo
          expStrNames = map getOccString expNames
      return expStrNames
    _ -> return []

-- | Util for printing
putString :: MonadIO m => String -> m ()
putString = liftIO . putStrLn
