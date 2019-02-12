let divisible_by_all x =
  [11; 12; 13; 14; 15; 16; 17; 18; 19; 20]
  |> List.for_all (fun n -> x mod n = 0)

let rec find_smallest n =
  if divisible_by_all n then n else find_smallest (n + 1)

let () = find_smallest 1 |> string_of_int |> print_endline
