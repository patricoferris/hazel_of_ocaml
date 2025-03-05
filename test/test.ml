open Ppxlib

let read_ast s =
  let f = Filename.temp_file "hazel-of-ocaml-" "" in
  (Out_channel.with_open_bin f @@ fun oc -> Out_channel.output_string oc s);
  match Driver.load_input ~kind:Impl ~input_name:f ~relocate:false f with
  | Ok (_, _, Impl ast) -> ast
  | Ok _ -> invalid_arg "Wanted an implementation file"
  | Error (e, _) -> Ppxlib.Location.Error.raise e

let ocaml_to_hazel s =
  let ast = read_ast s in
  let terms = List.map Hazel_of_ocaml.of_structure_item ast in
  let terms =
    List.fold_right (fun f exp -> f exp) terms Haz3lmenhir.AST.EmptyHole
  in
  Hazel_of_ocaml.Ppast.exp Format.std_formatter terms

let%expect_test "type" =
  ocaml_to_hazel {|type x = int|};
  [%expect {| type x = Int in ? |}]

let%expect_test "bindings" =
  ocaml_to_hazel {|let x = 1 |};
  [%expect {| let x = 1 in ? |}]

let%expect_test "simple function" =
  ocaml_to_hazel {|let f x = x|};
  [%expect {| let f = fun x -> x in ? |}]

let%expect_test "fibonacci" =
  ocaml_to_hazel
    {|let rec fib x = if x <= 1 then 1 else fib (x - 1) + fib (x - 2) |};
  [%expect
    {| let fib = fun x -> if x <= 1 then 1 else fib(x - 1) + fib(x - 2) in ? |}]

let%expect_test "list.hd" =
  ocaml_to_hazel
    {|let hd v = match v with hd :: _ -> hd | [] -> invalid_arg "empty list" |};
  [%expect
    {|
    let hd = fun v -> case v
      | hd :: _ => hd
      | [] => invalid_arg(?)
    end in ?
    |}]

let%expect_test "list.hd function" =
  ocaml_to_hazel
    {|let hd = function | hd :: _ -> hd | [] -> invalid_arg "empty list" |};
  [%expect
    {|
    let hd = fun x42 -> case x42
      | hd :: _ => hd
      | [] => invalid_arg(?)
    end in ?
    |}]

let%expect_test "sum definitions" =
  ocaml_to_hazel {|type t = A of int | B of string | C of int * string |};
  [%expect
    {|
    type t =
      + A(Int)
      + B(String)
      + C(Int, String)
     in ?
    |}]

let%expect_test "sum functions" =
  ocaml_to_hazel {|let f = function A 1 -> 0 | A i -> i | B -> 10 |};
  [%expect
    {|
    let f = fun x42 -> case x42
      | A(1) => 0
      | A(i) => i
      | B => 10
    end in ?
    |}]
