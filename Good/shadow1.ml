open StdLib

result =
  let a = 100 in
  let globalna (x : Int) = x + a in
  let lokalna (a : Int) = a + globalna a in
  (*let
    lokalna (x : Int) = let a = x in a + globalna a
  in*) lokalna 5

main = begin
    putStrLn $ show $ result
  end
