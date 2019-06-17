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
  end
