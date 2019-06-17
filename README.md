# WellTyped.hs
A fully-fledged interpreter of a simple functional programming language, written in Haskell.

### Introduction
A [well-typed interpreter](http://okmij.org/ftp/tagless-final/course/lecture.pdf) is an interpreter in which an Abstract Syntax Tree is well-typed in Haskell only if it corresponds to a well-typed program in the interpreted language. It is a commonly used example that demonstrates the power of languages with dependent types, for example [idris](http://docs.idris-lang.org/en/latest/tutorial/interp.html).

This is a research experiment to see how far can we go with the recently added [limited dependent typing capabilities](https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html) in Haskell. The main goal was to reproduce [this paper](https://pdfs.semanticscholar.org/5dae/20b002f4e9d91e60db6af192c69d7fe764c6.pdf) in Haskell.

### Features
 * [Hindley–Milner type system](#hindley-milner-type-system)
 * [Interacting with outside world using the IO monad](#interacting-with-outside-world-using-the-io-monad)
 * [Higher-order functions](#higher-order-functions)
 * [Lazy evaluation](#lazy-evaluation)
 * [Row-polymorphic object system](#row-polymorphic-object-system)
 * [Integration with Haskell's module system](#integration-with-haskells-module-system)

### Hindley-Milner type system
Everything you would expect from a language with [global type inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system). In most places you don't need to write types at all because they are inferred. This enables expressing algorithms in a clear and concise way. For example, quicksort can be written in just a few lines:
```ml
quicksort = let
  mkQuicksort quicksort li =
    if null li then [] else
    let
      lesser = tail li |> filter (< head li)
      greater = tail li |> filter (>= head li)
    in (quicksort lesser) ++ [head li] ++ (quicksort greater)
  in fix mkQuicksort
```
Note the use of the [fixpoint combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator) `fix mkQuicksort` to achieve recursion. To keep the core language simple and minimal, support for recursion is implemented through the `fix` function in the standard library.

### Interacting with outside world using the IO monad
We can use the IO [monad](https://en.wikipedia.org/wiki/Monad_%28functional_programming%29) to read from stdin and write to stdout. We enclose sequences of IO operations using `begin` and `end` keywords, similarly to Haskell's do-notation:
```ml
open StdLib

main = begin
    putStrLn "Hello World!"
    putStrLn "What's your name?"
    name : String <- getLine
    putStrLn <| "Nice to meet you, " ++ name ++ "!"
  end
```

### Higher-order functions
[Higher-order functions](https://en.wikipedia.org/wiki/Higher-order_function) are fully supported, which means that you can use familiar programming patterns from other functional languages, such as using map and fold to iterate over collections, writing functions in continuation-passing style to implement sophisticated control flow, etc. Here is a small example:
```ml
open StdLib

mkMap map = fun f li ->
  if null li then [] else f (head li) `cons` map f (tail li)

map = fix mkMap

mkConcat concat li =
  if null li then [] else head li ++ (concat $ tail li)

concat = fix mkConcat

functions = [(+1), (+3), ( * 3)]

main = begin
    putStrLn $ concat $ map (fun f -> show (f 2) ++ ", ") functions
    (* This should print 3, 5, 6 *)
  end
```

### Lazy evaluation
Lazy evaluation allows for manipulating potentially infinite data structures. Here is a program that creates infinite list of fibonacci numbers and later prints the first 10 of them:
```ml
open StdLib

fib = let
    mkFib fib = zipWith (+) ([1] ++ fib) ([0, 1] ++ fib)
  in fix mkFib
fib_prefix = take 10 fib

main = begin
    putStrLn $ "First 10 numbers of Fibonacci sequence"
    putStrLn $ show fib_prefix
  end
```

### Row-polymorphic object system
Ability to construct objects that model entities and their relationships is an essential element of any programming language. WellTyped.hs achieves this goal by having a [row-polymorphic](https://en.wikipedia.org/wiki/Row_polymorphism) object system, which allows expressing many data structures in a natural way.
```ml
open StdLib

encode token = "encoded " ++ token
secret = encode "secret_password"

HttpRequest url auth_token = object
  url = url
  headers = object
    www_authenticate = "Basic realm=dummy"
    authorization = encode auth_token
    content_type = "application/json"

isAuthenticated request = request.headers.authorization == secret

main = begin
    url : String <- getLine
    token : String <- getLine
    request = HttpRequest url token
    if isAuthenticated request then
      putStrLn "Authentication successful!"
    else
      putStrLn "Authentication failed!"
  end
```

### Integration with Haskell's module system
Programs written in WellTyped.hs can import modules written in Haskell. For example, standard library is implemented as a module that just exports relevant functions from Haskell's prelude: [StdLib.hs](Good/StdLib.hs)

For example, a relevant section that exports input and output operations looks like this:
```haskell
#include "extension_base.hs"
module StdLib where
import qualified Base
import Base (A, Type, eq_value, show_value)

io_ = [
    EXPORT("bindIO", FORALL(a, b), (>>=) :: IO a -> (a -> IO b) -> IO b),
    EXPORT("nextIO", FORALL(a, b), (>>) :: IO a -> IO b -> IO b),
    EXPORT("returnIO", FORALL(a), return :: a -> IO a),
    EXPORT("return", FORALL(a), return :: a -> IO a),
    EXPORT("failIO", FORALL(a), fail :: String -> IO a),
    EXPORT("mapM_", FORALL(a, b), mapM_ :: (a -> IO b) -> [a] -> IO ()),
    EXPORT("mapM", FORALL(a, b), mapM :: (a -> IO b) -> [a] -> IO [b]),
    EXPORT("sequence", FORALL(a), sequence_ :: [IO a] -> IO ()),
    EXPORT("sequence_", FORALL(a), sequence :: [IO a] -> IO [a]),

    EXPORT("putChar", NONE, putChar :: Char -> IO ()),
    EXPORT("putStr", NONE, putStr :: String -> IO ()),
    EXPORT("putStrLn", NONE, putStrLn :: String -> IO ()),
    EXPORT("print", FORALL(a), (putStrLn . show_value) :: A Type a => a -> IO ()),
    EXPORT("getChar", NONE, getChar :: IO Char),
    EXPORT("getLine", NONE, getLine :: IO String),
    EXPORT("getContents", NONE, getContents :: IO String),
    EXPORT("interact", NONE, interact :: (String -> String) -> IO ()),
    EXPORT("readFile", NONE, readFile :: FilePath -> IO String),
    EXPORT("writeFile", NONE, writeFile :: FilePath -> String -> IO ()),
    EXPORT("appendFile", NONE, appendFile :: FilePath -> String -> IO ())
  ]
```
