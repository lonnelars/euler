let check a b =
  let c = 1000 - a - b in
  (a * a) + (b * b) - (c * c) = 0

let rec solve a b =
  if b > 1000 then solve (a + 1) (a + 2)
  else if check a b then (a, b)
  else solve a (b + 1)

let () =
  let a, b = solve 1 2 in
  let c = 1000 - a - b in
  print_int a ;
  print_string ", " ;
  print_int b ;
  print_string ", " ;
  print_int c ;
  print_newline ()
