open Ppxlib
open Haz3lcore

module T = TermBase

let of_core_type (c : core_type) : TermBase.Typ.t = match c.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> IdTagged.fresh TermBase.Int
  | _ -> invalid_arg "TODO"

let of_type_declaration (td : type_declaration) = match td with
  | { ptype_manifest = Some c; ptype_kind = Ptype_abstract; _ } ->
    let term_typ = of_core_type c in
    let pat : T.tpat_term = T.Var "hello" in
    let ty_pat : T.tpat_t = IdTagged.fresh pat in
    let exp : T.exp_term = T.EmptyHole in
    IdTagged.fresh @@ TermBase.TyAlias (ty_pat, term_typ, IdTagged.fresh exp)
  | _ -> invalid_arg "TODO"

let of_structure_item (str : structure_item) = match str.pstr_desc with
  | Pstr_type (_, tds) ->
    let terms = List.map of_type_declaration tds in
    let ts : T.exp_term = T.ListLit terms in
    IdTagged.fresh ts
  | _ -> invalid_arg "TODO"
