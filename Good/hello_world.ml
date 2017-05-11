open StdLib

f = fun (x: a.a) -> [x, x]

main = begin
    putStrLn "Hello World!"
    putStrLn $
      (foldl (fun (a : [Char]) (b : Int) -> a ++ " | " ++ show b) "" [1, 2, 3])
  end
