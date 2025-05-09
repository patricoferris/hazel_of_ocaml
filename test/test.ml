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
  Hazel_of_ocaml.Typecheck.source := s;
  let terms =
    List.fold_right
      (fun item exp ->
        match Hazel_of_ocaml.of_structure_item ~typed:typecheck item exp with
        | Some f -> f
        | None -> exp)
      ast Hazel_of_ocaml.AST.EmptyHole
  in
  Hazel_of_ocaml.Ppast.exp Format.std_formatter terms

let test_unify a b =
  let t1 = Hazel_of_ocaml.Typecheck.parse_type a in
  let t2 = Hazel_of_ocaml.Typecheck.parse_type b in
  let unifs = Hazel_of_ocaml.Typecheck.unified_vars t1 t2 in
  let pp_type_parsed ppf v =
    Fmt.pf ppf "%a"
      (Hazel_of_ocaml.Ppast.typ ~parens:false)
      (Hazel_of_ocaml.Typecheck.type_parsed_to_type v)
  in
  Fmt.pr "Unified Variables: %a"
    Fmt.(list Fmt.(parens (pair ~sep:Fmt.comma string pp_type_parsed)))
    unifs

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
    end in
    ?
    |}]

let%expect_test "list.hd function + string concat" =
  ocaml_to_hazel
    {|let hd = function | hd :: _ -> hd | [] -> invalid_arg ("empty list" ^ "!") |};
  [%expect
    {|
    let hd = fun x1 -> case x1
      | hd :: _ => hd
      | [] => invalid_arg("empty list" ++ "!")
    end in
    ?
    |}]

let%expect_test "sum definitions" =
  ocaml_to_hazel {|type t = A of int | B of string | C of int * string |};
  [%expect
    {|
    type t =
      + A(Int)
      + B(String)
      + C((Int, String))
     in
    ?
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
      + C((Int, String))
     in
    let f : t -> Int = fun x2 -> case x2
      | A(1) => 0
      | A(i) => i
      | B => 10
    end in
    ?
    |}]

let%expect_test "lists" =
  ocaml_to_hazel ~typecheck:true
    {| let hd = function | x :: _ -> x | [] -> invalid_arg "Empty list" |};
  [%expect
    {|
    let hd : forall a -> [a] -> a = typfun a -> fun x3 -> case x3
      | x :: _ => x
      | [] => invalid_arg("Empty list")
    end in
    ?
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
    end in
    ?
    |}]

let%expect_test "type list with disable" =
  ocaml_to_hazel ~typecheck:true
    {|
  let rec length_aux len = function
      [] -> len
    | _::l -> length_aux (len + 1) l

  let length l = length_aux 0 l

  let len = length [1; 2; 3]
  |};
  [%expect
    {|
    let length_aux : forall a -> Int -> [a] -> Int = typfun a -> fun len -> fun x5 -> case x5
      | [] => len
      | _ :: l => length_aux@<a>(len + 1)(l)
    end in
    let length : forall a -> [a] -> Int = typfun a -> fun l -> length_aux@<a>(0)(l) in
    let len : Int = length@<Int>(1 :: 2 :: [3]) in ?
    |}]

let%expect_test "types" =
  ocaml_to_hazel ~typecheck:false
    {|
type expr =
  | VarX
  | VarY
  | Sine of expr
  | Cosine of expr
  | Average of expr* expr
  | Times of expr* expr
  | Thresh of expr* expr* expr* expr

let cosine e = Cosine e 

let is_cosine = function
  | Cosine _ -> true
  | _ -> false
|};
  [%expect
    {|
    type expr =
      + VarX
      + VarY
      + Sine(expr)
      + Cosine(expr)
      + Average((expr, expr))
      + Times((expr, expr))
      + Thresh((expr, expr, expr, expr))
     in
    let cosine = fun e -> Cosine(e) in let is_cosine = fun x6 -> case x6
      | Cosine(_) => true
      | _ => false
    end in
    ?
    |}]

(* Unification-esque *)
let%expect_test "simple unification" =
  test_unify "'a -> int" "int -> int";
  [%expect {| Unified Variables: (a, Int) |}]

let%expect_test "type constructors" =
  test_unify "'a list -> unit" "int list -> unit";
  [%expect {| Unified Variables: (a, Int) |}]

let%expect_test "type constructors 2" =
  test_unify "'a list -> ('a -> 'b) -> 'b list"
    "int list -> (int -> string) -> string list";
  [%expect {|
    Unified Variables: (a, Int)
    (b, String)
    |}]

let%expect_test "type vars" =
  test_unify "'a list -> 'b list" "'a list -> 'b list";
  [%expect {|
    Unified Variables: (a, a)
    (b, b)
    |}]

let%expect_test "type vars" =
  test_unify "('a -> 'b) -> 'a list -> 'b list"
    "('a -> 'b) -> 'a list -> 'b list";
  [%expect {|
    Unified Variables: (a, a)
    (b, b)
    |}]
