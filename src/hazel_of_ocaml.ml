open Ppxlib
module AST = AST
module Ppast = Ppast
module Typecheck = Typecheck

exception Unsupported of string

let unsupported s = raise (Unsupported s)

let rec of_core_type (c : core_type) : AST.typ =
  match c.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> AST.IntType
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> AST.StringType
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> AST.FloatType
  | Ptyp_tuple cs -> AST.TupleType (List.map of_core_type cs)
  | Ptyp_constr ({ txt = Lident typ; _ }, []) -> AST.TypVar typ
  | _ -> Fmt.invalid_arg "of_core_type: %a" Pprintast.core_type c

let is_disabled (attrs : attributes) =
  List.exists (fun (a : attribute) -> a.attr_name.txt = "hazel_disable") attrs

let of_type_declaration (td : type_declaration) =
  if is_disabled td.ptype_attributes then None
  else
    match td with
    | {
     ptype_manifest = Some c;
     ptype_kind = Ptype_abstract;
     ptype_name = { txt; _ };
     _;
    } ->
        let term_typ = of_core_type c in
        let ty_pat : AST.tpat = VarTPat txt in
        Some (fun exp -> AST.TyAlias (ty_pat, term_typ, exp))
    | {
     ptype_manifest = None;
     ptype_kind = Ptype_variant vs;
     ptype_name = { txt; _ };
     _;
    } ->
        let constructors : AST.sumterm list =
          List.map
            (fun (c : constructor_declaration) ->
              let args =
                match c.pcd_args with
                | Pcstr_tuple [] -> None
                | Pcstr_tuple l ->
                    Some (AST.TupleType (List.map of_core_type l))
                | Pcstr_record _ ->
                    failwith "Hazel does not support anonymous records"
              in
              AST.Variant (c.pcd_name.txt, args))
            vs
        in
        let typ_pat : AST.tpat = VarTPat txt in
        let sum = AST.SumTyp constructors in
        Some (fun exp -> AST.TyAlias (typ_pat, sum, exp))
    | _ ->
        Fmt.invalid_arg "of_type_declaration: %a" Pprintast.type_declaration td

let of_constant (c : constant) : AST.exp =
  match c with
  | Pconst_integer (l, _) -> Int (int_of_string l)
  | Pconst_float (l, _) -> Float (float_of_string l)
  | Pconst_string (s, _, _) -> String s
  | Pconst_char _ -> failwith "Characters not supported in Hazel"

let longident l =
  Longident.flatten_exn l.txt |> function
  | [ "List"; x ] | [ x ] -> x
  | vs ->
      Fmt.failwith "Hazel doesn't have a module system: %s"
        (String.concat "." vs)

let poly_vars v =
  let open Merlin_sherlodoc.Type_parsed in
  let rec loop acc = function
    | Tyvar v ->
        if not (List.exists (String.equal v) acc) then v :: acc else acc
    | Arrow (a, b) ->
        let v = loop acc a in
        loop v b
    | Tycon (_, lst) -> List.fold_left (fun acc v -> loop acc v) acc lst
    | _ -> []
  in
  loop [] v

let fresh_var =
  let i = ref 0 in
  fun () ->
    i := !i + 1;
    "x" ^ string_of_int !i

let ( let* ) v f = Option.bind v f

let merlin_position (loc : Ppxlib.location) =
  `Logical
    (loc.loc_start.pos_lnum, loc.loc_start.pos_cnum - loc.loc_start.pos_bol + 1)

let pp_pos ppf = function
  | `Logical (l, c) -> Fmt.pf ppf "%i:%i" l c
  | `Offset n -> Fmt.int ppf n
  | _ -> ()

let rec of_expression (e : expression) : AST.exp =
  let f =
    match e.pexp_desc with
    | Pexp_constant constant -> of_constant constant
    | Pexp_fun (lbl, _exp, pat, body) ->
        let label =
          match lbl with
          | Nolabel -> None
          | Labelled v -> Some v
          | Optional _ -> None
        in
        AST.Fun (of_pattern pat, of_expression body, label)
    | Pexp_function cases ->
        let hazel_cases = of_cases cases in
        let var = fresh_var () in
        AST.Fun (AST.VarPat var, AST.CaseExp (AST.Var var, hazel_cases), None)
    | Pexp_ident l -> AST.Var (longident l)
    | Pexp_ifthenelse (e1, e2, Some e3) ->
        AST.If (of_expression e1, of_expression e2, of_expression e3)
    | Pexp_match (exp, cases) -> AST.CaseExp (of_expression exp, of_cases cases)
    | Pexp_apply (e, args) -> (
        let hfunc = of_expression e in
        let hargs = List.map snd args |> List.map of_expression in
        match (hfunc, hargs) with
        | AST.Var "+", [ x; y ] -> AST.BinExp (x, IntOp Plus, y)
        | AST.Var "-", [ x; y ] -> AST.BinExp (x, IntOp Minus, y)
        (* Tricky: we don't have type information *)
        | AST.Var "<=", [ x; y ] -> AST.BinExp (x, IntOp LessThanOrEqual, y)
        | AST.Var "<", [ x; y ] -> AST.BinExp (x, IntOp LessThan, y)
        | AST.Var ">", [ x; y ] -> AST.BinExp (x, IntOp GreaterThan, y)
        | AST.Var ">=", [ x; y ] -> AST.BinExp (x, IntOp GreaterThanOrEqual, y)
        | AST.Var "*", [ x; y ] -> AST.BinExp (x, IntOp Times, y)
        | AST.Var "/", [ x; y ] -> AST.BinExp (x, IntOp Divide, y)
        | AST.Var "**", [ x; y ] -> AST.BinExp (x, IntOp Power, y)
        | AST.Var "=", [ x; y ] -> AST.BinExp (x, IntOp Equals, y)
        | AST.Var "<>", [ x; y ] -> AST.BinExp (x, IntOp NotEquals, y)
        | AST.Var "+.", [ x; y ] -> AST.BinExp (x, FloatOp Plus, y)
        | AST.Var "-.", [ x; y ] -> AST.BinExp (x, FloatOp Minus, y)
        (* Tricky: we don't have type information *)
        | AST.Var "<=.", [ x; y ] -> AST.BinExp (x, FloatOp LessThanOrEqual, y)
        | AST.Var "<.", [ x; y ] -> AST.BinExp (x, FloatOp LessThan, y)
        | AST.Var ">.", [ x; y ] -> AST.BinExp (x, FloatOp GreaterThan, y)
        | AST.Var ">=.", [ x; y ] ->
            AST.BinExp (x, FloatOp GreaterThanOrEqual, y)
        | AST.Var "*.", [ x; y ] -> AST.BinExp (x, FloatOp Times, y)
        | AST.Var "/.", [ x; y ] -> AST.BinExp (x, FloatOp Divide, y)
        | AST.Var "=.", [ x; y ] -> AST.BinExp (x, FloatOp Equals, y)
        | AST.Var "mod", [ x; y ] ->
            AST.ApExp (AST.Var "int_mod", AST.TupleExp [ x; y ])
        (* Strings *)
        | AST.Var "^", [ x; y ] -> AST.BinExp (x, StringOp Concat, y)
        | _ ->
            (* Trying our best with type application. We use Merlin to
               lookup the type of the arguments being supplied to this
               particular function application and reconstruct the unification
               of OCaml's type variables. *)
            let function_types =
              Typecheck.type_of_position ~label:"type application"
                (merlin_position e.pexp_loc)
            in
            (* Fmt.pr "Function types @%a %a : %a\n" pp_pos (merlin_position e.pexp_loc) Pprintast.expression e Fmt.(list ~sep:Fmt.comma Ppast.typ) (List.map Typecheck.type_parsed_to_type function_types);  *)
            let types =
              match function_types with
              | ft :: rt :: _ -> Some (ft, rt)
              | [ t ] ->
                  Fmt.failwith "Only one type %a" (Ppast.typ ~parens:false)
                    (Typecheck.type_parsed_to_type t)
              | [] -> None
            in
            (* Fmt.pr "Function Types: %a\n%!" Fmt.(list ~sep:Fmt.comma string) *)
            (*   (List.map (fun v -> Merlin_sherlodoc.Type_expr.normalize_type_parameters v |> Merlin_sherlodoc.Type_expr.to_string) function_types); *)
            let poly_variables, unified_variables =
              match types with
              | None -> ([], [])
              | Some (ft, rt) ->
                  (* Fmt.pr "Getting(%i): %a\n%!" e.pexp_loc.loc_start.pos_cnum Pprintast.expression e; *)
                  let unified_variables = Typecheck.unified_vars ft rt in
                  (poly_vars ft, unified_variables)
            in
            let needs_type_application =
              (* TODO: Only let-bound things need this, not higher-order function arguments. *)
              poly_variables <> []
              && not
                   (match hfunc with
                   | AST.Var "raise" | AST.Var "invalid_arg" -> true
                   | _ -> false)
            in
            if not needs_type_application then
              List.fold_right
                (fun acc f -> AST.ApExp (f, acc))
                (List.rev hargs) hfunc
            else
              let func =
                match hfunc with
                | AST.Var _function ->
                    List.fold_left
                      (fun acc (_, typ) ->
                        let typ = Typecheck.type_parsed_to_type typ in
                        AST.TypAp (acc, typ))
                      hfunc unified_variables
                | _ -> hfunc
              in
              List.fold_right
                (fun acc f -> AST.ApExp (f, acc))
                (List.rev hargs) func)
    | Pexp_construct (l, None) ->
        let construct_name = longident l in
        AST.Constructor (construct_name, AST.UnknownType EmptyHole)
    | Pexp_construct (l, Some e) ->
        let construct_name = longident l in
        let arg = of_expression e in
        let f = AST.Constructor (construct_name, AST.UnknownType EmptyHole) in
        AST.ApExp (f, arg)
    | Pexp_let (_rec, bindings, body) ->
        of_value_bindings bindings (of_expression body)
    | Pexp_tuple exps -> AST.TupleExp (List.map of_expression exps)
    | _ -> EmptyHole
  in
  f

and of_pattern (p : pattern) : AST.pat =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> VarPat txt
  | Ppat_construct (c, args) -> (
      match (longident c, args) with
      | "::", Some (_, { ppat_desc = Ppat_tuple [ hd; tl ]; _ }) ->
          AST.ConsPat (of_pattern hd, of_pattern tl)
      | _, Some (_, pat) -> AST.ApPat (AST.VarPat (longident c), of_pattern pat)
      | _, None -> AST.ConstructorPat (longident c, UnknownType Internal))
  | Ppat_any -> AST.WildPat
  | Ppat_constant c -> (
      match c with
      | Pconst_integer (l, _) -> AST.IntPat (int_of_string l)
      | Pconst_float (l, _) -> AST.FloatPat (float_of_string l)
      | Pconst_string (l, _, _) -> AST.StringPat l
      | Pconst_char _ -> failwith "Hazel does not support character patterns")
  | Ppat_tuple ts -> AST.TuplePat (List.map of_pattern ts)
  | _ -> EmptyHolePat

and of_cases (cs : case list) : (AST.pat * AST.exp) list =
  List.map (fun (c : case) -> (of_pattern c.pc_lhs, of_expression c.pc_rhs)) cs

and of_value_bindings ?(typed = false) bindings acc =
  let binding (v : value_binding) =
    if is_disabled v.pvb_attributes then None
    else
      Some
        (fun exp ->
          let type' =
            match typed with
            | false -> None
            | true -> (
                try
                  match v.pvb_pat.ppat_desc with
                  | Ppat_var _ -> (
                      let typs =
                        Typecheck.type_of_position
                          ~label:(Fmt.str "%a" Pprintast.binding v)
                          (merlin_position v.pvb_pat.ppat_loc)
                      in
                      let typ = List.hd typs in
                      let hazel_typ = Typecheck.type_parsed_to_type typ in
                      match poly_vars typ with
                      | [] -> Some ([], hazel_typ)
                      | vars ->
                          Some
                            ( vars,
                              List.fold_left
                                (fun t var ->
                                  AST.ForallType (AST.VarTPat var, t))
                                hazel_typ vars ))
                  | _ -> None
                with Not_found -> None (* | Invalid_argument v -> None *))
          in
          let pattern =
            match type' with
            | Some (_, ty) ->
                AST.CastPat (of_pattern v.pvb_pat, ty, UnknownType Internal)
            | None -> of_pattern v.pvb_pat
          in
          let e = of_expression v.pvb_expr in
          match (pattern, e) with
          | AST.CastPat (AST.VarPat _, _, _), AST.Fun _ -> (
              let typ =
                Typecheck.type_of_position ~label:"pattern"
                  (merlin_position v.pvb_pat.ppat_loc)
              in
              match poly_vars @@ List.hd typ with
              | [] -> AST.Let (pattern, e, exp)
              | vars ->
                  let e =
                    List.fold_left
                      (fun acc p -> AST.TypFun (AST.VarTPat p, acc))
                      e vars
                  in
                  AST.Let (pattern, e, exp))
          | _, _ -> AST.Let (pattern, e, exp))
  in
  List.fold_left
    (fun v b -> match binding b with Some f -> f v | None -> v)
    acc bindings

and arrow_type a b =
  let open Ocaml_typing.Outcometree in
  let a' = of_out_type a in
  match b with
  | Otyp_arrow (_, t0, t1) -> AST.ArrowType (a', arrow_type t0 t1)
  | _ -> AST.ArrowType (a', of_out_type b)

and of_out_type (v : Ocaml_typing.Outcometree.out_type) : AST.typ =
  match v with
  | Otyp_arrow (_, t0, t1) -> arrow_type t0 t1
  | Otyp_var (_, s) -> AST.TypVar s
  | Otyp_constr (p, []) -> (
      match p with
      | Oide_ident { printed_name = "int" } -> AST.IntType
      | Oide_ident { printed_name = "string" } -> AST.StringType
      | Oide_ident { printed_name = "float" } -> AST.FloatType
      | Oide_ident { printed_name = "bool" } -> AST.BoolType
      | Oide_ident { printed_name = p } -> AST.TypVar p
      | _ -> failwith "Modules not supported by hazel")
  | Otyp_constr (p, [ x ]) -> (
      match Fmt.str "%a" !Ocaml_typing.Oprint.out_ident p with
      | "list" -> AST.ArrayType (of_out_type x)
      | f -> invalid_arg ("Hazel doesn't have type constructors for " ^ f))
  | _ -> AST.UnknownType EmptyHole

let of_structure_item ?typed (str : structure_item) exp =
  match str.pstr_desc with
  | Pstr_type (_, tds) ->
      let terms = List.filter_map of_type_declaration tds in
      Some (List.fold_left (fun e f -> f e) exp terms)
  | Pstr_value (_, bindings) -> Some (of_value_bindings ?typed bindings exp)
  | _ ->
      Fmt.epr "Unsupported structure item:\n%a" Pprintast.structure_item str;
      None
