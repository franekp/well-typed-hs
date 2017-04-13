#include "extension_base.hs"
module StdLib where
import qualified Base
import Base (A, Type, eq_value, show_value)

bool_ = [
    EXPORT("True", NONE, True :: Bool),
    EXPORT("False", NONE, False :: Bool),
    EXPORT("&&", NONE, (&&) :: Bool -> Bool -> Bool),
    EXPORT("||", NONE, (||) :: Bool -> Bool -> Bool),
    EXPORT("not", NONE, not :: Bool -> Bool),
    EXPORT("<=", NONE, (<=) :: Int -> Int -> Bool),
    EXPORT("<", NONE, (<) :: Int -> Int -> Bool),
    EXPORT(">", NONE, (>) :: Int -> Int -> Bool),
    EXPORT(">=", NONE, (>=) :: Int -> Int -> Bool),
    EXPORT("==", FORALL(a), eq_value :: A Type a => a -> a -> Bool),
    EXPORT("!=", FORALL(a), (not .) . eq_value :: A Type a => a -> a -> Bool)
  ]

maybe_ = [
    EXPORT("Nothing", FORALL(a), Nothing :: Maybe a),
    EXPORT("Just", FORALL(a), Just :: a -> Maybe a),
    EXPORT("maybe", FORALL(a, b), maybe :: b -> (a -> b) -> Maybe a -> b)
  ]

either_ = [
    EXPORT("Left", FORALL(a, b), Left :: a -> Either a b),
    EXPORT("Right", FORALL(a, b), Right :: b -> Either a b),
    EXPORT("either", FORALL(a, b, c), either :: (a -> c) -> (b -> c) -> Either a b -> c)
  ]

string_ = [
    EXPORT("lines", NONE, lines :: String -> [String]),
    EXPORT("words", NONE, words :: String -> [String]),
    EXPORT("unlines", NONE, unlines :: [String] -> String),
    EXPORT("unwords", NONE, unwords :: [String] -> String),
    EXPORT("show", FORALL(a), show_value :: A Type a => a -> String)
  ]

int_ = [
    EXPORT("-", NONE, (-) :: Int -> Int -> Int),
    EXPORT("subtract", NONE, subtract :: Int -> Int -> Int),
    EXPORT("+", NONE, (+) :: Int -> Int -> Int),
    EXPORT("*", NONE, (*) :: Int -> Int -> Int),
    EXPORT("/", NONE, div :: Int -> Int -> Int),
    EXPORT("div", NONE, div :: Int -> Int -> Int),
    EXPORT("%", NONE, mod :: Int -> Int -> Int),
    EXPORT("mod", NONE, mod :: Int -> Int -> Int),
    EXPORT("**", NONE, (^) :: Int -> Int -> Int),
    EXPORT("min", NONE, min :: Int -> Int -> Int),
    EXPORT("max", NONE, max :: Int -> Int -> Int),
    EXPORT("succ", NONE, succ :: Int -> Int),
    EXPORT("pred", NONE, pred :: Int -> Int),
    EXPORT("enumFrom", NONE, enumFrom :: Int -> [Int]),
    EXPORT("enumFromThen", NONE, enumFromThen :: Int -> Int -> [Int]),
    EXPORT("enumFromTo", NONE, enumFromTo :: Int -> Int -> [Int]),
    EXPORT("enumFromThenTo", NONE, enumFromThenTo :: Int -> Int -> Int -> [Int]),
    EXPORT("minBound", NONE, minBound :: Int),
    EXPORT("maxBound", NONE, maxBound :: Int),
    EXPORT("abs", NONE, abs :: Int -> Int),
    EXPORT("signum", NONE, signum :: Int -> Int),
    EXPORT("negate", NONE, negate :: Int -> Int),
    EXPORT("even", NONE, even :: Int -> Bool),
    EXPORT("odd", NONE, odd :: Int -> Bool),
    EXPORT("gcd", NONE, gcd :: Int -> Int -> Int),
    EXPORT("lcm", NONE, lcm :: Int -> Int -> Int)
  ]

io_ = [
    EXPORT("bindIO", FORALL(a, b), (>>=) :: IO a -> (a -> IO b) -> IO b),
    EXPORT("nextIO", FORALL(a, b), (>>) :: IO a -> IO b -> IO b),
    EXPORT("returnIO", FORALL(a), return :: a -> IO a),
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

lookup_ :: A Type a => a -> [(a, b)] -> Maybe b
lookup_ k [] = Nothing
lookup_ k ((k_, v):t) = if k `eq_value` k_ then Just v else lookup_ k t

list_ = [
    EXPORT("cons", FORALL(a), (:) :: a -> [a] -> [a]),
    EXPORT("nil", FORALL(a), [] :: [a]),
    EXPORT("list", FORALL(a, b), ([a] -> b) -> (a -> b) -> b),

    EXPORT("foldr", FORALL(a, b), foldr :: (a -> b -> b) -> b -> [a] -> b),
    EXPORT("foldl", FORALL(a, b), foldl :: (b -> a -> b) -> b -> [a] -> b),
    EXPORT("foldr1", FORALL(a), foldr1 :: (a -> a -> a) -> [a] -> a),
    EXPORT("foldl1", FORALL(a), foldl1 :: (a -> a -> a) -> [a] -> a),
    EXPORT("null", FORALL(a), null :: [a] -> Bool),
    EXPORT("length", FORALL(a), length :: [a] -> Int),
    EXPORT("elem", FORALL(a), (\a -> any $ eq_value a) :: A Type a => a -> [a] -> Bool),
    EXPORT("maximum", NONE, maximum :: [Int] -> Int),
    EXPORT("minimum", NONE, minimum :: [Int] -> Int),
    EXPORT("sum", NONE, sum :: [Int] -> Int),
    EXPORT("product", NONE, product :: [Int] -> Int),

    EXPORT("map", FORALL(a, b), map :: (a -> b) -> [a] -> [b]),
    EXPORT("++", FORALL(a), (++) :: [a] -> [a] -> [a]),
    EXPORT("filter", FORALL(a), filter :: (a -> Bool) -> [a] -> [a]),
    EXPORT("head", FORALL(a), head :: [a] -> a),
    EXPORT("last", FORALL(a), last :: [a] -> a),
    EXPORT("tail", FORALL(a), tail :: [a] -> [a]),
    EXPORT("init", FORALL(a), init :: [a] -> [a]),
    EXPORT("at", FORALL(a), (!!) :: [a] -> Int -> a),
    EXPORT("reverse", FORALL(a), reverse :: [a] -> [a]),
    EXPORT("and", FORALL(a), and :: [Bool] -> Bool),
    EXPORT("or", FORALL(a), or :: [Bool] -> Bool),
    EXPORT("any", FORALL(a), any :: (a -> Bool) -> [a] -> Bool),
    EXPORT("all", FORALL(a), all :: (a -> Bool) -> [a] -> Bool),
    EXPORT("concat", FORALL(a), concat :: [[a]] -> [a]),
    EXPORT("concatMap", FORALL(a, b), concatMap :: (a -> [b]) -> [a] -> [b]),
    EXPORT("scanl", FORALL(a, b), scanl :: (b -> a -> b) -> b -> [a] -> [b]),
    EXPORT("scanl1", FORALL(a), scanl1 :: (a -> a -> a) -> [a] -> [a]),
    EXPORT("scanr", FORALL(a, b), scanr :: (a -> b -> b) -> b -> [a] -> [b]),
    EXPORT("scanr1", FORALL(a), scanr1 :: (a -> a -> a) -> [a] -> [a]),
    EXPORT("iterate", FORALL(a), iterate :: (a -> a) -> a -> [a]),
    EXPORT("repeat", FORALL(a), repeat :: a -> [a]),
    EXPORT("replicate", FORALL(a), replicate :: Int -> a -> [a]),
    EXPORT("cycle", FORALL(a), cycle :: [a] -> [a]),
    EXPORT("take", FORALL(a), take :: Int -> [a] -> [a]),
    EXPORT("drop", FORALL(a), drop :: Int -> [a] -> [a]),
    EXPORT("splitAt", FORALL(a), splitAt :: Int -> [a] -> ([a], [a])),
    EXPORT("takeWhile", FORALL(a), takeWhile :: (a -> Bool) -> [a] -> [a]),
    EXPORT("dropWhile", FORALL(a), dropWhile :: (a -> Bool) -> [a] -> [a]),
    EXPORT("span", FORALL(a), span :: (a -> Bool) -> [a] -> ([a], [a])),
    EXPORT("break", FORALL(a), break :: (a -> Bool) -> [a] -> ([a], [a])),
    EXPORT("notElem", FORALL(a), (\a -> (not .) $ any $ eq_value a) :: A Type a => a -> [a] -> Bool),
    EXPORT("lookup", FORALL(a, b), lookup_ :: A Type a => a -> [(a, b)] -> Maybe b),
    EXPORT("zip", FORALL(a, b), zip :: [a] -> [b] -> [(a, b)]),
    EXPORT("zip3", FORALL(a, b, c), zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]),
    EXPORT("zipWith", FORALL(a, b, c), zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]),
    EXPORT("zipWith3", FORALL(a, b, c, d), zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]),
    EXPORT("unzip", FORALL(a, b), unzip :: [(a, b)] -> ([a], [b])),
    EXPORT("unzip3", FORALL(a, b, c), unzip3 :: [(a, b, c)] -> ([a], [b], [c]))
  ]

func_ = [
    EXPORT("|>", FORALL(a, b), flip ($) :: a -> (a -> b) -> b),
    EXPORT("<|", FORALL(a, b), ($) :: (a -> b) -> a -> b),

    EXPORT("id", FORALL(a), id :: a -> a),
    EXPORT("const", FORALL(a, b), const :: a -> b -> a),
    EXPORT("<<", FORALL(a, b, c), (.) :: (b -> c) -> (a -> b) -> a -> c ),
    EXPORT("flip", FORALL(a, b, c), flip :: (a -> b -> c) -> b -> a -> c),
    EXPORT("$", FORALL(a, b), ($) :: (a -> b) -> a -> b),
    EXPORT("until", FORALL(a), until :: (a -> Bool) -> (a -> a) -> a -> a),
    EXPORT("asTypeOf", FORALL(a), asTypeOf :: a -> a -> a),
    EXPORT("error", FORALL(a), error :: [Char] -> a),
    EXPORT("undefined", FORALL(a), undefined :: a),
    EXPORT("seq", FORALL(a, b), seq :: a -> b -> b),
    EXPORT("$!", FORALL(a, b), ($!) :: (a -> b) -> a -> b)
  ]

tuple_ = [
    EXPORT("pair", FORALL(a, b), (,) :: a -> b -> (a, b)),
    EXPORT("triple", FORALL(a, b, c), (,,) :: a -> b -> c -> (a, b, c)),
    EXPORT("fst", FORALL(a, b), fst :: (a, b) -> a),
    EXPORT("snd", FORALL(a, b), snd :: (a, b) -> b)
  ]

module_exports = Base.ExtModule $
  bool_ ++ maybe_ ++ either_ ++ string_ ++ int_ ++ io_ ++ list_ ++ func_ ++ tuple_
