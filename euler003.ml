(* prime factorization by trial division *)
let prime_factorization x =
  let rec loop acc n x' =
    match x' with
    | 1 -> acc
    | _ ->
        if x' mod n = 0 then loop (n :: acc) 2 (x' / n)
        else loop acc (n + 1) x'
  in
  loop [] 2 x

let () =
  let prime_factors = prime_factorization 600851475143 in
  let max_factor = List.fold_left max (List.hd prime_factors) prime_factors in
  print_endline (string_of_int max_factor)
