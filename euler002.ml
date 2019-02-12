let sum_fibonacci_numbers limit =
  let rec loop acc a b =
    let next = a + b in
    if next > limit then acc
    else if next mod 2 = 0 then loop (acc + next) b next
    else loop acc b next
  in
  loop 2 1 2

let () = print_endline (string_of_int (sum_fibonacci_numbers 4000000))
