open StdLib

quicksort = let
  mkQuicksort (quicksort : [Int] -> [Int]) (li : [Int]) =
    if null li then [] else
    let
      lesser = tail li |> filter (< head li)
      greater = tail li |> filter (>= head li)
    in (quicksort lesser) ++ [head li] ++ (quicksort greater)
  in fix mkQuicksort

main = begin
    li = [5, 2, 4, 1, 3]
    putStrLn "Before quicksort"
    putStrLn $ show li
    putStrLn "After quicksort"
    putStrLn $ show $ quicksort li
  end
