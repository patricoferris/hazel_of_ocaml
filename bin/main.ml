open Ppxlib

let typecheck = Hazel_of_ocaml.Typecheck.typecheck
let usage_msg = "hazel_of_ocaml [-type] <input.ml>"
let type' = ref false
let input = ref None

let main type' input =
  let types =
    if not type' then None
    else
      let s = In_channel.with_open_bin input In_channel.input_all in
      let types = typecheck s in
      Some types
  in
  match Driver.load_input ~kind:Impl ~input_name:"" ~relocate:false input with
  | Ok (_, _, Impl ast) ->
      let terms = List.map (Hazel_of_ocaml.of_structure_item ?types) ast in
      let terms =
        List.fold_right (fun f exp -> f exp) terms Haz3lmenhir.AST.EmptyHole
      in
      Hazel_of_ocaml.Ppast.exp Format.std_formatter terms
  | Ok _ -> invalid_arg "Wanted an implementation file"
  | Error (e, _) -> Ppxlib.Location.Error.raise e

let () =
  let speclist =
    [ ("-type", Arg.Set type', "Try to type functions using Merlin") ]
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
  main !type' input
