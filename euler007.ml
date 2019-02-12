let rec sieve acc length limit n =
  if length = limit then acc
  else if List.exists (fun a -> n mod a = 0) acc then
    sieve acc length limit (n + 1)
  else sieve (n :: acc) (length + 1) limit (n + 1)

let () = sieve [] 0 10001 2 |> List.hd |> string_of_int |> print_endline
