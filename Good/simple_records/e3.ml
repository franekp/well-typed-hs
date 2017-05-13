open StdLib

four_letters_0 (a : a. a) (b : b. b) (c : c. c) (d : d. d) = object
  a = a
  b = b
  c = c
  d = d

two_letters_1 (a : a. a) (b : b. b) =
  fun (fa : aa. a -> aa) (fb : bb. b -> bb) ->
    object
      a = fa a
      b = fb b

two_letters_2 (fa : a. aa. a -> aa) (fb : b. bb. b -> bb) =
  fun (a : a) (b : b) ->
    object
      a = fa a
      b = fb b

result = object
  f = four_letters_0 1 2 3 4
  g = two_letters_1 1 2 (fun (a : Int) -> a + 1) (fun (a : a. a) -> a)
  h = two_letters_2 (fun (a : Int) -> a + 1) (fun (a : a. a) -> a) 2 3

main = putStrLn $ show $ result
