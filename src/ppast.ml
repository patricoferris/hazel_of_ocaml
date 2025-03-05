open Haz3lmenhir

type 'a pp = Format.formatter -> 'a -> unit

let hole fmt = Format.fprintf fmt "?"
let comma fmt () = Format.fprintf fmt ", "

let rec sumterm : AST.sumterm pp =
 fun fmt -> function
  | AST.Variant (lbl, None) -> Format.fprintf fmt "+ %s" lbl
  | AST.Variant (lbl, Some lst) ->
      Format.fprintf fmt "+ %s(%a)" lbl (typ ~parens:false) lst
  | _ -> failwith "Non-variant sumterms unsupported"

and typ ?(parens = true) : AST.typ pp =
 fun fmt -> function
  | TypVar v -> Format.pp_print_string fmt v
  | IntType -> Format.pp_print_string fmt "Int"
  | BoolType -> Format.pp_print_string fmt "Bool"
  | StringType -> Format.pp_print_string fmt "String"
  | FloatType -> Format.pp_print_string fmt "Float"
  | TupleType cs ->
      Format.fprintf fmt
        (if parens then "(%a)" else "%a")
        (Format.pp_print_list ~pp_sep:comma typ)
        cs
  | SumTyp s ->
      Format.fprintf fmt "@.@[<hov 2>  %a@]@."
        (Format.pp_print_list ~pp_sep:Format.pp_force_newline sumterm)
        s
  | _ -> hole fmt

let rec exp : AST.exp pp =
 fun fmt -> function
  | Int i -> Format.pp_print_int fmt i
  | TyAlias (p, t, e) ->
      Format.fprintf fmt "type %a = %a in %a" tpat p (typ ~parens:true) t exp e
  | EmptyHole -> hole fmt
  | Let (p, e1, e2) ->
      Format.fprintf fmt "let %a = %a in %a" (pat ~parens:true) p exp e1 exp e2
  | Fun (p, e, _) ->
      Format.fprintf fmt "fun %a -> %a" (pat ~parens:true) p exp e
  | Var v -> Format.pp_print_string fmt v
  | If (e1, e2, e3) ->
      Format.fprintf fmt "if %a then %a else %a" exp e1 exp e2 exp e3
  | ApExp (f, args) -> Format.fprintf fmt "%a(%a)" exp f exp args
  | TupleExp exps ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ")
           exp)
        exps
  | BinExp (x, op, y) ->
      let bin_op =
        match op with
        | IntOp Plus -> "+"
        | IntOp Minus -> "-"
        | IntOp LessThan -> "<"
        | IntOp LessThanOrEqual -> "<="
        | IntOp GreaterThan -> ">"
        | IntOp GreaterThanOrEqual -> ">"
        | IntOp Times -> "*"
        | IntOp Divide -> "/"
        | IntOp Power -> "**"
        | IntOp Equals -> "=="
        | IntOp NotEquals -> "!="
        | _ -> "?"
      in
      Format.fprintf fmt "%a %s %a" exp x bin_op exp y
  | CaseExp (e, cases) ->
      Format.fprintf fmt "case %a @.@[<hov 2>  %a@]@.end" exp e
        (Format.pp_print_list ~pp_sep:Format.pp_force_newline case)
        cases
  | _ -> hole fmt

and case : (AST.pat * AST.exp) pp =
 fun fmt (p, e) -> Format.fprintf fmt "| %a => %a" (pat ~parens:true) p exp e

and tpat : AST.tpat pp =
 fun fmt -> function VarTPat v -> Format.pp_print_string fmt v | _ -> hole fmt

and pat ?(parens = true) : AST.pat pp =
 fun fmt v ->
  let patt = pat ~parens in
  match v with
  | VarPat v -> Format.pp_print_string fmt v
  | ConstructorPat (c, UnknownType Internal) -> Format.fprintf fmt "%s" c
  | ApPat (f, arg) -> Format.fprintf fmt "%a(%a)" patt f (pat ~parens:false) arg
  | IntPat i -> Format.pp_print_int fmt i
  | ConsPat (hd, tl) -> Format.fprintf fmt "%a :: %a" patt hd patt tl
  | WildPat -> Format.fprintf fmt "_"
  | TuplePat pats ->
      Format.fprintf fmt
        (if parens then "(%a)" else "%a")
        Format.(pp_print_list ~pp_sep:comma pat)
        pats
  | _ -> hole fmt
