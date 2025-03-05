open Ppxlib
open Haz3lmenhir
module Ppast = Ppast

let rec of_core_type (c : core_type) : AST.typ =
  match c.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> AST.IntType
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> AST.StringType
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> AST.FloatType
  | Ptyp_tuple cs -> AST.TupleType (List.map of_core_type cs)
  | _ -> invalid_arg "TODO"

let of_type_declaration (td : type_declaration) =
  match td with
  | {
   ptype_manifest = Some c;
   ptype_kind = Ptype_abstract;
   ptype_name = { txt; _ };
   _;
  } ->
      let term_typ = of_core_type c in
      let ty_pat : AST.tpat = VarTPat txt in
      fun exp -> AST.TyAlias (ty_pat, term_typ, exp)
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
              | Pcstr_tuple l -> Some (AST.TupleType (List.map of_core_type l))
              | Pcstr_record _ ->
                  failwith "Hazel does not support anonymous records"
            in
            AST.Variant (c.pcd_name.txt, args))
          vs
      in
      let typ_pat : AST.tpat = VarTPat txt in
      let sum = AST.SumTyp constructors in
      fun exp -> AST.TyAlias (typ_pat, sum, exp)
  | _ -> invalid_arg "TODO"

let of_constant (c : constant) : AST.exp =
  match c with
  | Pconst_integer (l, _) -> Int (int_of_string l)
  | Pconst_float (l, _) -> Float (float_of_string l)
  | Pconst_string (s, _, _) -> String s
  | Pconst_char _ -> failwith "Characters not supported in Hazel"

let longident l =
  Longident.flatten_exn l.txt |> function
  | [ x ] -> x
  | _ -> failwith "Hazel doesn't have a module system"

let rec of_expression (e : expression) : AST.exp =
  match e.pexp_desc with
  | Pexp_constant constant -> of_constant constant
  | Pexp_function (params, _constraint, body) ->
      let only_vals =
        List.filter_map
          (function
            | { pparam_desc = Pparam_val (label, _, pat); _ } ->
                Some
                  ( of_pattern pat,
                    match label with
                    | Nolabel -> None
                    | Labelled v -> Some v
                    | _ -> failwith "Optional labels not supported" )
            | _ -> failwith "Unsupported")
          params
      in
      let body =
        match body with
        | Pfunction_body b -> of_expression b
        | Pfunction_cases (cs, _, _) ->
            let cases = of_cases cs in
            AST.Fun (AST.VarPat "x42", AST.CaseExp (AST.Var "x42", cases), None)
      in
      List.fold_left (fun acc (p, lbl) -> AST.Fun (p, acc, lbl)) body only_vals
  | Pexp_ident l -> AST.Var (longident l)
  | Pexp_ifthenelse (e1, e2, Some e3) ->
      AST.If (of_expression e1, of_expression e2, of_expression e3)
  | Pexp_match (exp, cases) -> AST.CaseExp (of_expression exp, of_cases cases)
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
      | _ ->
          let args =
            match args with
            | [] -> failwith "No arguments!"
            | [ x ] -> x
            | args -> AST.TupleExp args
          in
          AST.ApExp (func, args))
  | _ -> EmptyHole

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

let of_structure_item (str : structure_item) =
  match str.pstr_desc with
  | Pstr_type (_, tds) ->
      let terms = List.map of_type_declaration tds in
      fun exp -> List.fold_left (fun e f -> f e) exp terms
  | Pstr_value (_, bindings) ->
      let binding (v : value_binding) =
       fun exp -> AST.Let (of_pattern v.pvb_pat, of_expression v.pvb_expr, exp)
      in
      let f acc = List.fold_left (fun v b -> (binding b) v) acc bindings in
      fun exp -> f exp
  | _ -> invalid_arg "TODO"
