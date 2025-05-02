open Merlin_kernel

let find_type name (sign : Ocaml_typing.Types.signature) =
  let open Ocaml_typing.Types in
  match
    List.find_map
      (function
        | Sig_value (v, t, _) ->
            if Ocaml_typing.Ident.equal v name then Some t else None
        | _ -> None)
      sign
  with
  | None -> None
  | Some v -> Some v.val_type

let source = ref ""

let pp_query_result ppf (_loc, v, _tail) =
  Fmt.pf ppf "%s" (match v with `Index i -> string_of_int i | `String s -> s)

let pp_pos ppf = function
  | `Offset o -> Fmt.pf ppf "off:%i" o
  | _ -> Fmt.pf ppf "position"

let parse_type typ =
  Lexing.from_string typ
  |> Merlin_sherlodoc.Type_parser.main Merlin_sherlodoc.Type_lexer.token

let query_result_to_type = function
  | _, `String typ, _ ->
      Lexing.from_string typ
      |> Merlin_sherlodoc.Type_parser.main Merlin_sherlodoc.Type_lexer.token
      |> Option.some
  | _ -> None

let rec length (v : Merlin_sherlodoc.Type_parsed.t) : int =
  match v with Arrow (a, b) -> length a + length b | _ -> 1

let cmp_length a b = Int.compare (length b) (length a)

let type_of_position ?(label = "=") position =
  ignore label;
  let source = Msource.make !source in
  let config = Mconfig.initial in
  let pipeline = Mpipeline.make config source in
  Mpipeline.with_pipeline pipeline @@ fun () ->
  let query = Query_protocol.Type_enclosing (None, position, None) in
  let results = Query_commands.dispatch pipeline query in
  (* Fmt.pr "ToP Results: %a\n%!" Fmt.(list ~sep:Fmt.comma pp_query_result) results; *)
  List.filter_map query_result_to_type results |> List.stable_sort cmp_length

let typecheck expr =
  Mocaml.with_state (Mocaml.new_state ()) @@ fun () ->
  let config = Mconfig.initial in
  Mocaml.setup_typer_config config;
  let source = Msource.make expr in
  let Mreader.{ parsetree; _ } = Mreader.parse config (source, None) in
  let types = Mtyper.run config parsetree in
  let typedtree = Mtyper.get_typedtree types in
  match typedtree with
  | `Implementation impl -> impl.str_type
  | _ -> assert false

let rec type_parsed_to_type : Merlin_sherlodoc.Type_parsed.t -> AST.typ =
  function
  | Arrow (a, b) -> AST.ArrowType (type_parsed_to_type a, type_parsed_to_type b)
  | Tyvar v -> AST.TypVar v
  | Tuple ts -> AST.TupleType (List.map type_parsed_to_type ts)
  | Tycon ("string", _) -> AST.StringType
  | Tycon ("int", _) -> AST.IntType
  | Tycon ("float", _) -> AST.FloatType
  | Tycon ("list", [ a ]) -> AST.ArrayType (type_parsed_to_type a)
  | Tycon (s, []) -> AST.TypVar s
  | v ->
      Fmt.failwith "Unknown type: %s"
        Merlin_sherlodoc.(
          Type_expr.to_string @@ Type_expr.normalize_type_parameters v)

let unified_vars a b =
  if cmp_length a b <> 0 then []
  else
    let rec loop acc (a : Merlin_sherlodoc.Type_parsed.t)
        (b : Merlin_sherlodoc.Type_parsed.t) =
      match (a, b) with
      | Tyvar v1, (Tyvar _ as v2) -> (v1, v2) :: acc
      | Tyvar v1, (Tycon (_, _args) as t) -> (v1, t) :: acc
      | Tycon (_, args1), Tycon (_, args2) ->
          let combined =
            try List.combine args1 args2
            with Invalid_argument _ as e ->
              Fmt.epr "args1: %a\nargs2: %a\n"
                Fmt.(list ~sep:Fmt.comma Ppast.typ)
                (List.map type_parsed_to_type args1)
                Fmt.(list ~sep:Fmt.comma Ppast.typ)
                (List.map type_parsed_to_type args2);
              raise e
          in
          List.fold_left (fun acc (t1, t2) -> loop acc t1 t2) acc combined
      | Arrow (a1, b1), Arrow (a2, b2) ->
          let acc' = loop acc a1 a2 in
          loop acc' b1 b2
      | _ -> acc
    in
    loop [] a b
    |> List.fold_left (fun acc v -> if List.mem v acc then acc else v :: acc) []
    |> List.stable_sort (fun (s1, _) (s2, _) -> String.compare s1 s2)
