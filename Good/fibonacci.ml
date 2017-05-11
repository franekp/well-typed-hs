open StdLib

fib = let
    mkFib (fib : [Int]) = zipWith (+) ([1] ++ fib) ([0, 1] ++ fib)
  in fix mkFib
fib_prefix = take 10 fib

main = begin
    putStrLn "Hello World!"
    putStrLn $ "First 10 of Fibonacci sequence: " ++ (show fib_prefix)
    putStrLn $
      (foldl (fun (a : [Char]) (b : Int) -> a ++ " | " ++ show b) "" [1, 2, 3])
  end
