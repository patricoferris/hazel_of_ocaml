open Ppxlib
open Haz3lcore

module T = TermBase

let main () =
  match Driver.load_input ~kind:Impl ~input_name:"" ~relocate:false  Sys.argv.(1) with
  | Ok (_, _, Impl ast) ->(
    let terms = List.map Hazel_of_ocaml.of_structure_item ast in
    List.iter (T.pp_exp_t Format.std_formatter) terms
  )
  | _ -> invalid_arg "TODO"

let () = main ()
