open StdLib

mkMap (map111 : a. b. (a -> b) -> [a] -> [b]) = fun (f : a -> b) (li : [a]) ->
  if null li then [] else f (head li) `cons` map111 f (tail li)

map111 = fix mkMap

mkConcat (concat111 : a. [[a]] -> [a]) (li : [[a]]) =
  if null li then [] else head li ++ (concat111 $ tail li)

concat111 = fix mkConcat

functions = [(+1), (+3), ( * 3)]

main = begin
    putStrLn $ show $ map111 (fun (f : Int -> Int) -> f 2) functions
  end
