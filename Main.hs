module Main where

import System.IO (stdin, hGetContents)
import System.Environment (getArgs, getProgName)

import Syntax.LexGrammar
import Syntax.ParGrammar
import Syntax.AbsGrammar
import Syntax.LayoutGrammar

import Syntax.ErrM

import Syntax.Translate (transModule)
import Semantics.Exec (uast_to_string, uast_to_io)
import Semantics.AlgorithmW (run_algorithm_w)
import ExtLoader.Loader (load_ext_modules)
import ExtLoader.GetTypes (get_ext_module_types)
import Base (ExtModuleEnv)

type ParseFun a = [Token] -> Err a

myLLexer = resolveLayout True . myLexer

runFile :: ParseFun (Pos Module) -> FilePath -> IO ()
runFile p f = {-putStrLn f >>-} readFile f >>= run p f

run :: ParseFun (Pos Module) -> String -> String -> IO ()
run p filename s = let ts = myLLexer s in case p ts of
  Bad s -> do
    putStrLn "\nParse              Failed...\n"
    putStrLn "Tokens:"
    putStrLn $ show ts
    putStrLn s
  Ok tree -> do
    --putStrLn "\nParse Successful!"
    let uast = transModule (filename, s) tree
    module_env <- load_ext_modules uast
    let module_type_env = get_ext_module_types module_env
    let uast' = run_algorithm_w module_type_env uast
    uast_to_io module_env uast'
    --putStrLn $ uast_to_string module_env uast

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No input files."
    --[] -> hGetContents stdin >>= run pModule
    "-s":fs -> mapM_ (runFile pModule) fs
    fs -> mapM_ (runFile pModule) fs
