let primes n =
  let numbers =
    let rec loop acc n =
      match n with 1 -> acc | _ -> loop (n :: acc) (n - 1)
    in
    loop [] n
  in
  let limit = int_of_float (sqrt (float_of_int n)) in
  let rec sieve acc sieve_list =
    match sieve_list with
    | [] -> acc
    | h :: t ->
        if h > limit then List.sort compare (acc @ sieve_list)
        else sieve (h :: acc) (List.filter (fun x -> x mod h != 0) t)
  in
  sieve [] numbers

let () =
  primes 2000000 |> List.fold_left ( + ) 0 |> string_of_int |> print_endline
