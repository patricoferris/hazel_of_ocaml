type t = A of int | B of string | C of int * string

let to_int = function
  | A i -> i
  | B s -> int_of_string s
  | C (i, s) -> i + int_of_string s
