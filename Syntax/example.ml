open SomeModule

something = let
    app1 = fun (x : a. a) (f : b. a -> b) -> f a
    app1 (x : a. a) (f : b. a -> b) = f a
    const = 5
    open Bla
  in (app1 const Add) const

double_quantf = fun (ff : a. b. a -> b) -> 2

main = 4

bla = let Monad = IO in begin
  a <- readLine
  b = a
  putStrLn b

some_object = object
  a = 3
  double (i : Int) = i + i

some_record = {a = 2, b = 3, c = 4,}
some_list = [1, 2, 3,]
some_tuple = (1, 2, 3, 4,)
