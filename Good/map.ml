open StdLib

mkMap (map111 : x. x) = fun (f : y. y) (li : z. z) ->
  if null li then [] else f (head li) `cons` map111 f (tail li)

map111 = fix mkMap

mkConcat (concat111 : a. a) (li : b. b) =
  if null li then [] else head li ++ (concat111 $ tail li)

concat111 = fix mkConcat

functions = [(+1), (+3), ( * 3)]

main = begin
    putStrLn $ show $ map111 (fun (f : f. f) -> f 2) functions
  end
