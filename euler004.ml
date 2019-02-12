(* Solve by brute force. Calculate all products, then find the ones that are palindromes, then find the maximum value. *)

let all_products =
  let rec loop acc a b =
    match (a, b) with
    | 999, 999 -> (999 * 999) :: acc
    | _, 999 -> loop ((a * 999) :: acc) (a + 1) 100
    | _, _ -> loop ((a * b) :: acc) a (b + 1)
  in
  loop [] 100 100

let reverse_string s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])

let is_palindrome x =
  let s = string_of_int x in
  reverse_string s = s

let () =
  all_products |> List.filter is_palindrome |> List.fold_left max 0
  |> string_of_int |> print_endline
