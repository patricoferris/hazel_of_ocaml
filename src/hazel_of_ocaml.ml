open Ppxlib
open Haz3lmenhir
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
  let open Ocaml_typing.Outcometree in
  let rec loop acc = function
    | Otyp_var (_, v) ->
        if not (List.exists (String.equal v) acc) then v :: acc else acc
    | Otyp_arrow (_, a, b) ->
        let v = loop acc a in
        loop v b
    | Otyp_constr (_, lst) -> List.fold_left (fun acc v -> loop acc v) acc lst
    | _ -> []
  in
  loop [] v

let fresh_var =
  let i = ref 0 in
  fun () ->
    i := !i + 1;
    "x" ^ string_of_int !i

let is_function s (p : AST.pat) =
  match p with AST.VarPat s' -> String.equal s s' | _ -> false

let find_polyvars (func : string) polyvars =
  List.find_map
    (fun (pat, v) -> if is_function func pat then Some v else None)
    polyvars

let rec of_expression ~polyvars (e : expression) : AST.exp =
  let of_expression = of_expression ~polyvars in
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
        let hazel_cases = of_cases ~polyvars cases in
        let var = fresh_var () in
        AST.Fun (AST.VarPat var, AST.CaseExp (AST.Var var, hazel_cases), None)
    | Pexp_ident l -> AST.Var (longident l)
    | Pexp_ifthenelse (e1, e2, Some e3) ->
        AST.If (of_expression e1, of_expression e2, of_expression e3)
    | Pexp_match (exp, cases) ->
        AST.CaseExp (of_expression exp, of_cases ~polyvars cases)
    | Pexp_apply (e, args) -> (
        let func = of_expression e in
        let args = List.map snd args |> List.map of_expression in
        match (func, args) with
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
            (* Trying our best with type application *)
            let func =
              match func with
              | AST.Var funcs -> (
                  match find_polyvars funcs !polyvars with
                  | None | Some [] -> func
                  | Some polyvars ->
                      List.fold_right
                        (fun v acc -> AST.TypAp (acc, AST.TypVar v))
                        polyvars func)
              | _ -> func
            in
            List.fold_right
              (fun acc f -> AST.ApExp (f, acc))
              (List.rev args) func)
    | Pexp_construct (l, None) ->
        let construct_name = longident l in
        AST.Constructor (construct_name, AST.UnknownType EmptyHole)
    | Pexp_construct (l, Some e) ->
        let construct_name = longident l in
        let arg = of_expression e in
        let f = AST.Constructor (construct_name, AST.UnknownType EmptyHole) in
        AST.ApExp (f, arg)
    | Pexp_let (_rec, bindings, body) ->
        of_value_bindings ~polyvars bindings (of_expression body)
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

and of_cases ~polyvars (cs : case list) : (AST.pat * AST.exp) list =
  List.map
    (fun (c : case) -> (of_pattern c.pc_lhs, of_expression ~polyvars c.pc_rhs))
    cs

and of_value_bindings ?types ~polyvars bindings acc =
  let binding (v : value_binding) =
    if is_disabled v.pvb_attributes then None
    else
      Some
        (fun exp ->
          let type' =
            match types with
            | None -> None
            | Some types -> (
                try
                  match v.pvb_pat.ppat_desc with
                  | Ppat_var { txt; _ } -> (
                      let t =
                        Typecheck.find_function_type
                          (Ocaml_typing.Ident.create_local txt)
                          types
                      in
                      Ocaml_typing.Printtyp.reset ();
                      let outcome =
                        Ocaml_typing.Printtyp.tree_of_type_scheme t
                      in
                      let t' = of_out_type outcome in
                      match find_polyvars txt !polyvars with
                      | None | Some [] -> Some ([], t')
                      | Some vars ->
                          Some
                            ( vars,
                              List.fold_left
                                (fun t var ->
                                  AST.ForallType (AST.VarTPat var, t))
                                t' vars ))
                  | _ -> None
                with Not_found -> None (* | Invalid_argument v -> None *))
          in
          let pattern =
            match type' with
            | Some (_, ty) ->
                AST.CastPat (of_pattern v.pvb_pat, ty, UnknownType Internal)
            | None -> of_pattern v.pvb_pat
          in
          let e = of_expression ~polyvars v.pvb_expr in
          match (pattern, e) with
          | AST.CastPat (AST.VarPat p, _, _), AST.Fun _ -> (
              match find_polyvars p !polyvars with
              | None | Some [] -> AST.Let (pattern, e, exp)
              | Some vars ->
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

and collect_polyvars types polyvars bs =
  let bindings (v : value_binding) =
    match types with
    | None -> ()
    | Some types -> (
        match v.pvb_pat.ppat_desc with
        | Ppat_var { txt; _ } -> (
            let t =
              Typecheck.find_function_type
                (Ocaml_typing.Ident.create_local txt)
                types
            in
            Ocaml_typing.Printtyp.reset ();
            let outcome = Ocaml_typing.Printtyp.tree_of_type_scheme t in
            let vars = poly_vars outcome in
            match vars with
            | [] -> ()
            | vars -> polyvars := (of_pattern v.pvb_pat, vars) :: !polyvars)
        | _ -> ())
  in
  List.iter bindings bs

let of_structure_item ?types ~polyvars (str : structure_item) exp =
  match str.pstr_desc with
  | Pstr_type (_, tds) ->
      let terms = List.filter_map of_type_declaration tds in
      Some (List.fold_left (fun e f -> f e) exp terms)
  | Pstr_value (_, bindings) ->
      (* let pp_binding fmt (v : value_binding) = *)
      (*   Fmt.pf fmt "%a %a\n%!" Pprintast.pattern v.pvb_pat Pprintast.expression v.pvb_expr *)
      (* in *)
      (* Fmt.pr "Dealing with \n %a \n%!" Fmt.(list pp_binding) bindings; *)
      Some (of_value_bindings ?types bindings ~polyvars exp)
  | _ ->
      Fmt.epr "Unsupported structure item:\n%a" Pprintast.structure_item str;
      None

let collect_polyvars ?types ~polyvars (str : structure) =
  let apply str =
    match str.pstr_desc with
    | Pstr_value (_, bindings) -> collect_polyvars types polyvars bindings
    | _ -> ()
  in
  List.iter apply str
