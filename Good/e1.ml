
get_a (re : r. a. {r | a : a}) = re.a
get_b (re : r. b. {r | b : b}) = re.b

get_newa (re : r. a. {r | newa : a}) = re.newa
get_newb (re : r. b. {r | newb : b}) = re.newb

get_a_and_b (re : a. b. r. {r | a : a, b : b}) = {
  newa = get_a re,
  newb = get_b re,
}

get_a_and_b_revised (re : r. b. a. {r | newa : a, newb : b}) = {
  a = get_newa re,
  b = get_newb re,
}

main = get_a_and_b_revised (get_a_and_b ({
  a = 4,
  b = {ba = 2, bb = 3},
}))
