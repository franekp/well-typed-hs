

three_letters_0 (a : a. a) (b : b. b) (c : c. c) = object
  a = a
  b = b
  c = c

three_letters_1 (a : a. a) (b : b. b) (c : c. c) =
  fun (fa : aa. a -> aa) (fb : bb. b -> bb) (fc : cc. c -> cc) ->
    object
      a = fa a
      b = fb b
      c = fc c

three_letters_2 (fa : a. aa. a -> aa) (fb : b. bb. b -> bb) (fc : c. cc. c -> cc) =
  fun (a : a) (b : b) (c : c) ->
    object
      a = fa a
      b = fb b
      c = fc c

main = object
  f = three_letters_0 1 2 3
  g = three_letters_1
    1 2 3
    (fun (a : Int) -> a + 1)
    (fun (a : a. a) -> a)
    (fun (a : Int) -> a + a)
  h = three_letters_2
    (fun (a : Int) -> a + 1)
    (fun (a : a. a) -> a)
    (fun (a : Int) -> a + a)
    1 2 3
