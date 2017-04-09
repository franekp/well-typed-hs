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
import GHC.Paths (libdir)
import Name (getOccString)
import DynFlags
import Data.Dynamic (fromDyn, fromDynamic)

import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

import Base

load_ext_modules :: UAst l -> IO ExtModuleEnv
load_ext_modules = load_ext_modules_by_name . dump_module_names

dump_module_names :: UAst l -> [String]
dump_module_names (LiteralUA _) = []
dump_module_names (AppUA f a) = dump_module_names f ++ dump_module_names a
dump_module_names (LambdaUA (_, _) a) = dump_module_names a
dump_module_names (VarUA _) = []
dump_module_names (LetUA (_, a) b) = dump_module_names a ++ dump_module_names b
dump_module_names (RecordNilUA) = []
dump_module_names (RecordConsUA (_, a) rest) = dump_module_names a ++ dump_module_names rest
dump_module_names (RecordGetUA _ a) = dump_module_names a
dump_module_names (OpenUA m a) = (m:dump_module_names a)

load_ext_modules_by_name :: [String] -> IO ExtModuleEnv
load_ext_modules_by_name modules = do
  script_path <- getExecutablePath
  let script_dir = takeDirectory script_path
  runGhc (Just libdir) $ do
    putString ":::"
    modSums <- initSession modules script_dir
    putString ":::"
    imports <- mapM parseImportDecl ["import qualified " ++ m | m <- modules]
    setContext [IIDecl i | i <- imports]
    dynVals <- mapM dynCompileExpr [m ++ ".module_exports" | m <- modules]
    let res = [(fromDynamic d :: Maybe ExtModule) | d <- dynVals]
    let f (mod, a) = case a of {Just m -> m ; Nothing -> error $ "wrong type of " ++ mod ++ ".module_exports"}
    return $ ExtModuleEnv $ zip modules $ map f (zip modules res)

-- | Init interactive session and load modules
initSession modStrNames script_dir = do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ flip xopt_set Opt_Cpp $ dflags {
    --hscTarget = HscInterpreted
    ghcLink   = LinkInMemory
    , importPaths = importPaths dflags ++ [script_dir]
    , includePaths = includePaths dflags ++ [script_dir </> "ExtLoader"]
    }
  targets <- mapM
              (\modStrName -> do
                  putString modStrName
                  target <- guessTarget ("*"++modStrName++".hs") Nothing
                  return target
              ) modStrNames
  setTargets targets
  load LoadAllTargets
  modSums <- mapM
              (\modStrName -> do
                  putString modStrName
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
putString = liftIO . putStrLn