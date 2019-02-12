let ( -- ) a b =
  let rec loop acc c =
    let c' = c - 1 in
    if a >= c then acc else loop (c' :: acc) c'
  in
  loop [] b

let square x = x * x

let sum_of_squares n = 1 -- n |> List.map square |> List.fold_left ( + ) 0

let square_of_sum n = 1 -- n |> List.fold_left ( + ) 0 |> square

let () =
  square_of_sum 101 - sum_of_squares 101 |> string_of_int |> print_endline
