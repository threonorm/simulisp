open Errors
open Location

type name = string
module NameEnv = Map.Make (struct type t = name let compare = compare end)

type sop =
  | SAdd | SMinus | SMult | SDiv | SPower (*int*)
  | SEqual | SLess | SLeq | SGreater | SGeq (*bool*)

type static_exp_desc =
  | SInt of int
  | SBool of bool
  | SVar of name
  | SBinOp of sop * static_exp * static_exp
  | SIf of static_exp * static_exp * static_exp (* se1 ? se2 : se3 *)

and static_exp =
    { se_desc : static_exp_desc;
      se_loc : location }

type static_ty = STInt | STBool

let mk_static_exp ?(loc = no_location) desc =
  { se_desc = desc; se_loc = loc }
let mk_static_var s =
  mk_static_exp (SVar s)
let mk_static_int i =
  mk_static_exp (SInt i)
let mk_static_bool b =
  mk_static_exp (SBool b)
