open Merlin_kernel

let find_function_type name (sign : Ocaml_typing.Types.signature) =
  let open Ocaml_typing.Types in
  match
    List.find_map
      (function
        | Sig_value (v, t, _) ->
            if Ocaml_typing.Ident.equal v name then Some t else None
        | _ -> None)
      sign
  with
  | None -> raise Not_found
  | Some v -> (
      match get_desc v.val_type with
      | Tarrow _ -> v.val_type
      | _ -> invalid_arg (Ocaml_typing.Ident.name name ^ " is not a function"))

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
