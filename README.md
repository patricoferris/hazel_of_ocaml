hazel_of_ocaml
--------------

Generate Hazel programs from OCaml programs. See [the tests](./test/test.ml) for some examples.

```sh
$ cat test.ml
type t = A of int | B of string | C of int * string

let to_int = function
  | A i -> i
  | B s -> int_of_string s
  | C (i, s) -> i + int_of_string s
$ hazel_of_ocaml test.ml
type t =
+ A(Int)
+ B(String)
+ C(Int, String) in let to_int = fun x42 -> case x42
  | A(i) => i
  | B(s) => int_of_string(s)
  | C(i, s) => i + int_of_string(s)
end in ?
```
