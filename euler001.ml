let range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux (j - 1) []

let () =
  range 0 1000
  |> List.filter (fun i -> i mod 3 = 0 || i mod 5 = 0)
  |> List.fold_left ( + ) 0 |> string_of_int |> print_endline
