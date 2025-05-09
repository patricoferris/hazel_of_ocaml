(* Copied from Hazel's Menhir parser 

MIT License

Copyright (c) 2016-2024

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. *)

type filter_action = Pause | Debug | Hide | Eval

type op_bin_float =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equals
  | NotEquals

type op_bin_bool = And | Or

type op_bin_int =
  | Plus
  | Minus
  | Times
  | Power
  | Divide
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equals
  | NotEquals

type op_bin_string = Concat | Equals

type bin_op =
  | IntOp of op_bin_int
  | FloatOp of op_bin_float
  | StringOp of op_bin_string
  | BoolOp of op_bin_bool

type op_un_meta = Unquote
type op_un_int = Minus
type op_un_bool = Not
type op_un = Meta of op_un_meta | Int of op_un_int | Bool of op_un_bool
type typ_provenance = Internal | EmptyHole
type tpat = InvalidTPat of string | EmptyHoleTPat | VarTPat of string

type typ =
  | IntType
  | StringType
  | FloatType
  | BoolType
  | SumTyp of sumtype
  | UnknownType of typ_provenance
  | TupleType of typ list
  | ArrayType of typ
  | ArrowType of typ * typ
  | TypVar of string
  | InvalidTyp of string
  | ForallType of tpat * typ
  | RecType of tpat * typ

and sumterm = Variant of string * typ option | BadEntry of typ
and sumtype = sumterm list

type pat =
  | CastPat of pat * typ * typ
  | EmptyHolePat
  | WildPat
  | IntPat of int
  | FloatPat of float
  | VarPat of string
  | ConstructorPat of string * typ
  | StringPat of string
  | TuplePat of pat list
  | BoolPat of bool
  | ConsPat of pat * pat
  | ListPat of pat list
  | ApPat of pat * pat
  | InvalidPat of string

type if_consistency = Consistent | Inconsistent
type deferral_pos = InAp | OutsideAp

type exp =
  | Int of int
  | Float of float
  | Var of string
  | Constructor of string * typ
  | String of string
  | ListExp of exp list
  | TupleExp of exp list
  | BinExp of exp * bin_op * exp
  | UnOp of op_un * exp
  | Let of pat * exp * exp
  | Fun of pat * exp * string option
  | CaseExp of exp * (pat * exp) list
  | ApExp of exp * exp
  | FixF of pat * exp
  | Bool of bool
  | Cast of exp * typ * typ
  | FailedCast of exp * typ * typ
  | EmptyHole
  | Filter of filter_action * exp * exp
  | BuiltinFun of string
  | Undefined
  | Seq of exp * exp
  | Test of exp
  | Deferral
  | TypFun of tpat * exp
  | Cons of exp * exp
  | ListConcat of exp * exp
  | If of exp * exp * exp
  | InvalidExp of string
  | TypAp of exp * typ
  | DynamicErrorHole of exp * string
  | TyAlias of tpat * typ * exp
