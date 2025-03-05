open Ppxlib

let main () =
  match
    Driver.load_input ~kind:Impl ~input_name:"" ~relocate:false Sys.argv.(1)
  with
  | Ok (_, _, Impl ast) ->
      let terms = List.map Hazel_of_ocaml.of_structure_item ast in
      let terms =
        List.fold_right (fun f exp -> f exp) terms Haz3lmenhir.AST.EmptyHole
      in
      Hazel_of_ocaml.Ppast.exp Format.std_formatter terms
  | Ok _ -> invalid_arg "Wanted an implementation file"
  | Error (e, _) -> Ppxlib.Location.Error.raise e

let () = main ()
