open Ppxlib

let typecheck = Hazel_of_ocaml.Typecheck.typecheck
let usage_msg = "hazel_of_ocaml [-type|-both] <input.ml>"
let type' = ref false
let both = ref false
let input = ref None

let main type' both input =
  let types =
    if type' then
      let s = In_channel.with_open_bin input In_channel.input_all in
      let types = typecheck s in
      Some types
    else None
  in
  match Driver.load_input ~kind:Impl ~input_name:"" ~relocate:false input with
  | Ok (_, _, Impl ast) ->
      let polyvars = ref [] in
      Hazel_of_ocaml.collect_polyvars ?types ~polyvars ast;
      let terms =
        List.fold_right
          (fun item exp ->
            match
              Hazel_of_ocaml.of_structure_item ?types ~polyvars item exp
            with
            | Some f -> f
            | None -> exp)
          ast Haz3lmenhir.AST.EmptyHole
      in
      let more_terms =
        if both && type' then
          List.fold_right
            (fun item exp ->
              match
                Hazel_of_ocaml.of_structure_item ?types:None ~polyvars item exp
              with
              | Some f -> f
              | None -> exp)
            ast terms
        else if both && not type' then (
          let types =
            let s = In_channel.with_open_bin input In_channel.input_all in
            let types = typecheck s in
            Some types
          in
          let polyvars = ref [] in
          Hazel_of_ocaml.collect_polyvars ?types ~polyvars ast;
          List.fold_right
            (fun item exp ->
              match
                Hazel_of_ocaml.of_structure_item ?types ~polyvars item exp
              with
              | Some f -> f
              | None -> exp)
            ast terms)
        else terms
      in
      Hazel_of_ocaml.Ppast.exp Format.std_formatter more_terms
  | Ok _ -> invalid_arg "Wanted an implementation file"
  | Error (e, _) -> Ppxlib.Location.Error.raise e

let () =
  let speclist =
    [
      ("-type", Arg.Set type', "Try to type functions using Merlin");
      ( "-both",
        Arg.Set both,
        "Generate a typed and untyped version of the input program" );
    ]
  in
  let () =
    Arg.parse speclist
      (fun i ->
        match !input with
        | None -> input := Some i
        | _ -> failwith "Only one input")
      usage_msg
  in
  let input = Option.get !input in
  main !type' !both input
