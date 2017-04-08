module Main where

import System.IO (stdin, hGetContents)
import System.Environment (getArgs, getProgName)

import Syntax.LexGrammar
import Syntax.ParGrammar
import Syntax.PrintGrammar
import Syntax.AbsGrammar
import Syntax.LayoutGrammar

import Syntax.ErrM

import Syntax.Translate (transModule)
import Semantics.Exec (uast_to_string)

type ParseFun a = [Token] -> Err a

myLLexer = resolveLayout True . myLexer

runFile :: ParseFun Module -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun Module -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
  Bad s -> do
    putStrLn "\nParse              Failed...\n"
    putStrLn "Tokens:"
    putStrLn $ show ts
    putStrLn s
  Ok tree -> do
    putStrLn "\nParse Successful!"
    evalModule tree

evalModule :: Module -> IO ()
evalModule m = do
  putStrLn $ uast_to_string $ transModule m

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No input files."
    --[] -> hGetContents stdin >>= run pModule
    "-s":fs -> mapM_ (runFile pModule) fs
    fs -> mapM_ (runFile pModule) fs
