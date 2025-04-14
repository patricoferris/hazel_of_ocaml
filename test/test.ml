open Ppxlib

let read_ast s =
  let f = Filename.temp_file "hazel-of-ocaml-" "" in
  (Out_channel.with_open_bin f @@ fun oc -> Out_channel.output_string oc s);
  match Driver.load_input ~kind:Impl ~input_name:f ~relocate:false f with
  | Ok (_, _, Impl ast) -> ast
  | Ok _ -> invalid_arg "Wanted an implementation file"
  | Error (e, _) -> Ppxlib.Location.Error.raise e

let ocaml_to_hazel ?(typecheck = false) s =
  let ast = read_ast s in
  let types =
    if typecheck then Some (Hazel_of_ocaml.Typecheck.typecheck s) else None
  in
  let polyvars = ref [] in
  Hazel_of_ocaml.collect_polyvars ?types ~polyvars ast;
  let terms =
    List.fold_right
      (fun item exp ->
        match Hazel_of_ocaml.of_structure_item ?types ~polyvars item exp with
        | Some f -> f
        | None -> exp)
      ast Haz3lmenhir.AST.EmptyHole
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
      | [] => invalid_arg("empty list")
    end in ?
    |}]

let%expect_test "list.hd function" =
  ocaml_to_hazel
    {|let hd = function | hd :: _ -> hd | [] -> invalid_arg "empty list" |};
  [%expect
    {|
    let hd = fun x1 -> case x1
      | hd :: _ => hd
      | [] => invalid_arg("empty list")
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
  ocaml_to_hazel ~typecheck:true
    {|
  type t = A of int | B of string | C of int * string
  let f = function A 1 -> 0 | A i -> i | B -> 10 |};
  [%expect
    {|
    type t =
      + A(Int)
      + B(String)
      + C(Int, String)
     in let f : t -> Int = fun x2 -> case x2
      | A(1) => 0
      | A(i) => i
      | B => 10
    end in ?
    |}]

let%expect_test "lists" =
  ocaml_to_hazel ~typecheck:true
    {| let hd = function | x :: _ -> x | [] -> invalid_arg "Empty list" |};
  [%expect
    {|
    let hd : forall a -> [a] -> a = typfun a -> fun x3 -> case x3
      | x :: _ => x
      | [] => invalid_arg("Empty list")
    end in ?
    |}]

let%expect_test "another list" =
  ocaml_to_hazel ~typecheck:true
    {|
let[@tail_mod_cons] rec mapi i f = function
  | [] -> []
  | [ a1 ] ->
      let r1 = f i a1 in
      [ r1 ]
  | a1 :: a2 :: l ->
      let r1 = f i a1 in
      let r2 = f (i + 1) a2 in
      r1 :: r2 :: mapi (i + 2) f l
|};
  [%expect
    {|
    let mapi : forall a -> forall b -> Int -> (Int -> a -> b) -> [a] -> [b] = typfun a -> typfun b -> fun i -> fun f -> fun x4 -> case x4
      | [] => []
      | [a1] => let r1 = f(i)(a1) in [r1]
      | a1 :: a2 :: l =>
          let r1 = f(i)(a1) in let r2 = f(i + 1)(a2) in r1 :: r2 :: mapi@<a>@<b>(i + 2)(f)(l)
    end in ?
    |}]

let%expect_test "type list with disable" =
  ocaml_to_hazel ~typecheck:true
    {|
  let rec length_aux len = function
      [] -> len
    | _::l -> length_aux (len + 1) l

  let length l = length_aux 0 l
  |};
  [%expect
    {|
    let length_aux : forall a -> Int -> [a] -> Int = typfun a -> fun len -> fun x5 -> case x5
      | [] => len
      | _ :: l => length_aux@<a>(len + 1)(l)
    end in let length : forall a -> [a] -> Int = typfun a -> fun l -> length_aux@<a>(0)(l) in ? |}]
