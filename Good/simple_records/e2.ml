open StdLib

four_letters_0 (a : a. a) (b : b. b) (c : c. c) (d : d. d) = object
  a = a
  b = b
  c = c
  d = d

four_letters_1 (a : a. a) (b : b. b) (c : c. c) (d : d. d) =
  fun (fa : aa. a -> aa) (fb : bb. b -> bb) (fc : cc. c -> cc) (fd : dd. d -> dd) ->
    object
      a = fa a
      b = fb b
      c = fc c
      d = fd d

four_letters_2 (fa : a. aa. a -> aa) (fb : b. bb. b -> bb) (fc : c. cc. c -> cc) (fd : d. dd. d -> dd) =
  fun (a : a) (b : b) (c : c) (d : d) ->
    object
      a = fa a
      b = fb b
      c = fc c
      d = fd d

result = object
  f = four_letters_0 1 2 3 4
  g = four_letters_1
    1 2 3 4
    (fun (a : Int) -> a + 1)
    (fun (a : a. a) -> a)
    (fun (a : Int) -> a + a)
    (fun (a : a. a) -> a)
  h = four_letters_2
    (fun (a : Int) -> a + 1)
    (fun (a : a. a) -> a)
    (fun (a : Int) -> a + a)
    (fun (a : a. a) -> a)
    1 2 3 4

main = putStrLn $ show $ result
