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
  + C(Int, String)
 in let to_int = fun x1 -> case x1
  | A(i) => i
  | B(s) => int_of_string(s)
  | C(i, s) => i + int_of_string(s)
end in ?
```

You can pass the `-type` flag to `hazel_of_ocaml` and it will try to add type annotations to your functions.

```sh
$ hazel_of_ocaml -type test.ml
type t =
  + A(Int)
  + B(String)
  + C(Int, String)
 in let to_int : t -> Int = fun x1 -> case x1
  | A(i) => i
  | B(s) => int_of_string(s)
  | C(i, s) => i + int_of_string(s)
end in ?
```
