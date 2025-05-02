open Ppxlib

let usage_msg = "hazel_of_ocaml [-type|-both] <input.ml>"
let type' = ref false
let both = ref false
let input = ref None

let main typed both input =
  (if typed then
     let s = In_channel.with_open_bin input In_channel.input_all in
     Hazel_of_ocaml.Typecheck.source := s);
  match Driver.load_input ~kind:Impl ~input_name:"" ~relocate:false input with
  | Ok (_, _, Impl ast) ->
      let terms =
        List.fold_right
          (fun item exp ->
            match Hazel_of_ocaml.of_structure_item ~typed item exp with
            | Some f -> f
            | None -> exp)
          ast Hazel_of_ocaml.AST.EmptyHole
      in
      let more_terms =
        if both && typed then
          List.fold_right
            (fun item exp ->
              match Hazel_of_ocaml.of_structure_item ~typed:false item exp with
              | Some f -> f
              | None -> exp)
            ast terms
        else if both && not typed then (
          let s = In_channel.with_open_bin input In_channel.input_all in
          Hazel_of_ocaml.Typecheck.source := s;
          List.fold_right
            (fun item exp ->
              match Hazel_of_ocaml.of_structure_item ~typed:true item exp with
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
