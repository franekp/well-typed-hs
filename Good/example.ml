
get_a (re : r. a. {r | a : a}) = re.a
get_b (re : r. b. {r | b : b}) = re.b

get_a_and_b (re : a. b. r. {r | a : a, b : b}) = {
  newa = get_a re,
  newb = get_b re,
}

main = get_a_and_b ({
  a = 4,
  b = {ba = 2, bb = 3},
})
