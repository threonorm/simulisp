open Ast
open Static
open Misc

exception Fallback

type 'a it_funs = {
  static_exp : 'a it_funs -> 'a -> static_exp -> static_exp * 'a;
  static_exp_desc : 'a it_funs -> 'a -> static_exp_desc -> static_exp_desc * 'a;
  ty : 'a it_funs -> 'a -> ty -> ty * 'a;
  link : 'a it_funs -> 'a -> link -> link * 'a;
  edesc : 'a it_funs -> 'a -> edesc -> edesc * 'a;
  exp : 'a it_funs -> 'a -> exp -> exp * 'a;
  pat : 'a it_funs -> 'a -> pat -> pat * 'a;
  equation : 'a it_funs -> 'a -> equation -> equation * 'a;
  var_dec : 'a it_funs -> 'a -> var_dec -> var_dec * 'a;
  block : 'a it_funs -> 'a -> block -> block * 'a;
  node_dec : 'a it_funs -> 'a -> node_dec -> node_dec * 'a;
  const_dec : 'a it_funs -> 'a -> const_dec -> const_dec * 'a;
  program : 'a it_funs -> 'a -> program -> program * 'a;
}

let rec exp_it funs acc e = funs.exp funs acc e
and exp funs acc e =
  let e_desc, acc = edesc_it funs acc e.e_desc in
  let e_ty, acc = ty_it funs acc e.e_ty in
  { e with e_desc = e_desc; e_ty = e_ty }, acc

and edesc_it funs acc ed =
  try funs.edesc funs acc ed
  with Fallback -> edesc funs acc ed
and edesc funs acc ed = match ed with
  | Econst v -> Econst v, acc
  | Evar id -> Evar id, acc
  | Ereg e ->
      let e, acc = exp_it funs acc e in
      Ereg e, acc
  | Emem(k, addr_size, word_size, s, args) ->
      let addr_size, acc = static_exp_it funs acc addr_size in
      let word_size, acc = static_exp_it funs acc word_size in
      let args, acc = mapfold (exp_it funs) acc args in
      Emem(k, addr_size, word_size, s, args), acc
  | Ecall(id, params, args) ->
      let params, acc = mapfold (static_exp_it funs) acc params in
      let args, acc = mapfold (exp_it funs) acc args in
      Ecall(id, params, args), acc

and static_exp_it funs acc sd =
  try funs.static_exp funs acc sd
  with Fallback -> static_exp funs acc sd
and static_exp funs acc se =
  let se_desc, acc = static_exp_desc_it funs acc se.se_desc in
  { se with se_desc = se_desc }, acc

and static_exp_desc_it funs acc sed =
  try funs.static_exp_desc funs acc sed
  with Fallback -> static_exp_desc funs acc sed
and static_exp_desc funs acc sed = match sed with
  | SInt _ | SBool _ | SVar _ -> sed, acc
  | SBinOp (sop, se1, se2) ->
      let se1, acc = static_exp_it funs acc se1 in
      let se2, acc = static_exp_it funs acc se2 in
      SBinOp(sop, se1, se2), acc
  | SIf(c, se1, se2) ->
      let c, acc = static_exp_it funs acc c in
      let se1, acc = static_exp_it funs acc se1 in
      let se2, acc = static_exp_it funs acc se2 in
      SIf(c, se1, se2), acc

and ty_it funs acc t = try funs.ty funs acc t with Fallback -> ty funs acc t
and ty funs acc t = match t with
  | TUnit | TBit -> t, acc
  | TBitArray se ->
      let se, acc = static_exp_it funs acc se in
      TBitArray se, acc
  | TProd t_l ->
      let t_l, acc = mapfold (ty_it funs) acc t_l in
      TProd t_l, acc
  | TVar link ->
      let link_v, acc = link_it funs acc !link in
      link := link_v;
      TVar link, acc

and link_it funs acc c =
  try funs.link funs acc c
  with Fallback -> link funs acc c
and link funs acc l = match l with
  | TIndex _ -> l, acc
  | TLink ty ->
      let ty, acc = ty_it funs acc ty in
      TLink ty, acc

and pat_it funs acc p =
  try funs.pat funs acc p
  with Fallback -> pat funs acc p
and pat funs acc p = p, acc

and equation_it funs acc eq = funs.equation funs acc eq
and equation funs acc (pat, e) =
  let pat, acc = pat_it funs acc pat in
  let e, acc = exp_it funs acc e in
  (pat, e), acc

and block_it funs acc b =
  try funs.block funs acc b
  with Fallback -> block funs acc b
and block funs acc b = match b with
  | BEqs(eqs, vds) ->
      let vds, acc = mapfold (var_dec_it funs) acc vds in
      let eqs, acc = mapfold (equation_it funs) acc eqs in
      BEqs (eqs, vds), acc
  | BIf(se, b1, b2) ->
      let se, acc = static_exp_it funs acc se in
      let b1, acc = block_it funs acc b1 in
      let b2, acc = block_it funs acc b2 in
      BIf(se, b1, b2), acc

and var_dec_it funs acc vd = funs.var_dec funs acc vd
and var_dec funs acc vd =
  let v_ty, acc = ty_it funs acc vd.v_ty in
  { vd with v_ty = v_ty }, acc


and node_dec_it funs acc nd = funs.node_dec funs acc nd
and node_dec funs acc nd =
  let n_inputs, acc = mapfold (var_dec_it funs) acc nd.n_inputs in
  let n_outputs, acc = mapfold (var_dec_it funs) acc nd.n_outputs in
  let n_constraints, acc = mapfold (static_exp_it funs) acc nd.n_constraints in
  let n_body, acc = block_it funs acc nd.n_body in
  { nd with
      n_inputs = n_inputs;
      n_outputs = n_outputs;
      n_body = n_body;
      n_constraints = n_constraints }
  , acc


and const_dec_it funs acc c = funs.const_dec funs acc c
and const_dec funs acc c =
  let c_value, acc = static_exp_it funs acc c.c_value in
  { c with c_value = c_value }, acc

and program_it funs acc p = funs.program funs acc p
and program funs acc p =
  let p_consts, acc = mapfold (const_dec_it funs) acc p.p_consts in
  let p_nodes, acc = mapfold (node_dec_it funs) acc p.p_nodes in
  { p_nodes = p_nodes; p_consts = p_consts }, acc


let defaults = {
  static_exp = static_exp;
  static_exp_desc = static_exp_desc;
  ty = ty;
  link = link;
  edesc = edesc;
  exp = exp;
  pat = pat;
  equation = equation;
  var_dec = var_dec;
  block = block;
  node_dec = node_dec;
  const_dec = const_dec;
  program = program;
}
