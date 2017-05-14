open StdLib

zrob (x : Int) = let z = x in fun (a : Int) -> a + z

f1 = zrob 5
f2 = zrob 7

main = begin
    putStrLn $ show $ (f1 3, f2 3)
  end
