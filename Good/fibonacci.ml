open StdLib

fib = let
    mkFib fib = zipWith (+) ([1] ++ fib) ([0, 1] ++ fib)
  in fix mkFib
fib_prefix = take 10 fib

main = begin
    putStrLn $ "First 10 numbers of Fibonacci sequence"
    putStrLn $ show fib_prefix
  end
