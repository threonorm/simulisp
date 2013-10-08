open Ast
open Mapfold
open Static
open Static_utils
open Location
open Errors

(** Inlines all nodes with static paramaters. *)

let expect_bool env se =
  let se = simplify env se in
  match se.se_desc with
    | SBool v -> v
    | _ -> Format.eprintf "Expected a boolean@."; raise Error

let expect_int env se =
  let se = simplify env se in
  match se.se_desc with
    | SInt v -> v
    | _ -> Format.eprintf "Expected an integer@."; raise Error

let simplify_ty env ty = match ty with
  | TBitArray se -> TBitArray (simplify env se)
  | _ -> ty

(** Find a node by name*)
let nodes_list = ref []
let find_node f =
  List.find (fun n -> f = n.n_name) !nodes_list

let vars_of_pat env pat =
  let exp_of_ident id =
    try
      let ty = IdentEnv.find id env in
      mk_exp ~ty:ty (Evar id)
    with
      | Not_found -> Format.eprintf "Not in env: %a@." Ident.print_ident id; assert false
  in
  let rec _vars_of_pat acc pat = match pat with
    | Evarpat id -> (exp_of_ident id)::acc
    | Etuplepat l -> List.fold_left (fun acc id -> (exp_of_ident id)::acc) acc l
  in
    _vars_of_pat [] pat

let ident_of_exp e = match e.e_desc with
  | Evar x -> x
  | _ -> assert false

let rename env vd =
  let e = mk_exp ~ty:vd.v_ty (Evar (Ident.copy vd.v_ident)) in
  IdentEnv.add vd.v_ident e env

let build_params m names values =
  List.fold_left2 (fun m { p_name = n } v -> NameEnv.add n v m) m names values

let build_exp m vds values =
  List.fold_left2 (fun m { v_ident = n } e -> IdentEnv.add n e m) m vds values

let build_env env vds =
  List.fold_left (fun env vd -> IdentEnv.add vd.v_ident vd.v_ty env) env vds

let rec find_local_vars b = match b with
  | BEqs (_, vds) -> vds
  | BIf (_, trueb, falseb) -> (find_local_vars trueb) @ (find_local_vars falseb)

(** Substitutes idents with new names, static params with their values *)
let do_subst_block m subst b  =
  let translate_ident subst id =
    try
      ident_of_exp (IdentEnv.find id subst)
    with
      | Not_found -> id
  in
  let static_exp funs (subst, m) se =
    simplify m se, (subst, m)
  in
  let exp funs (subst, m) e =
    let e, _ = Mapfold.exp funs (subst, m) e in
    match e.e_desc with
    | Evar x ->
        let e = if IdentEnv.mem x subst then IdentEnv.find x subst else e in
        e, (subst, m)
    | _ -> Mapfold.exp funs (subst, m) e
  in
  let pat funs (subst, m) pat = match pat with
    | Evarpat id -> Evarpat (translate_ident subst id), (subst, m)
    | Etuplepat ids -> Etuplepat (List.map (translate_ident subst) ids), (subst, m)
  in
  let var_dec funs (subst, m) vd =
    (* iterate on the type *)
    let vd, _ = Mapfold.var_dec funs (subst, m) vd in
    { vd with v_ident = translate_ident subst vd.v_ident }, (subst, m)
  in
  let funs =
    { Mapfold.defaults with static_exp = static_exp; exp = exp;
      pat = pat; var_dec = var_dec }
  in
  let b, _ = Mapfold.block_it funs (subst, m) b in
  b

let check_params loc m param_names params cl =
  let env = build_params NameEnv.empty param_names params in
  let cl = List.map (simplify env) cl in
  try
    check_true m cl
  with Unsatisfiable(c) ->
    Format.eprintf "%aThe following constraint is not satisfied: %a@."
      print_location loc  Printer.print_static_exp c;
    raise Error

let rec inline_node loc env m call_stack f params args pat =
  (* Check that the definition is sound *)
  if List.mem (f, params) call_stack then (
    Format.eprintf "The definition of %s is circular.@." f;
    raise Error
  );
  let call_stack = (f, params)::call_stack in

  (* do the actual work *)
  let n = find_node f in
  check_params loc m n.n_params params n.n_constraints;
  let m = build_params m n.n_params params in
  let subst = build_exp IdentEnv.empty n.n_inputs args in
  let subst = build_exp subst n.n_outputs (List.rev (vars_of_pat env pat)) in
  let locals = find_local_vars n.n_body in
  let subst = List.fold_left rename subst locals in
  let b = do_subst_block m subst n.n_body in
  let b = Normalize.block b in
  b, call_stack

and translate_eq env m subst call_stack (eqs, vds) ((pat, e) as eq) =
  match e.e_desc with
    (* Inline all nodes  or only those with params or declared inline
       if no_inline_all = true *)
    | Ecall(f, params, args) ->
        (try
            let n = find_node f in
            if not !Cli_options.no_inline_all
              || not (Misc.is_empty params)
              || n.n_inlined = Inlined then
              let params = List.map (simplify m) params in
              let b, call_stack = inline_node e.e_loc env m call_stack f params args pat in
              let new_eqs, new_vds = translate_block env m subst call_stack b in
              new_eqs@eqs, new_vds@vds
            else
              eq::eqs, vds
          with
            | Not_found -> eq::eqs, vds (* Predefined function*)
        )
    | _ -> eq::eqs, vds

and translate_eqs env m subst call_stack acc eqs =
  List.fold_left (translate_eq env m subst call_stack) acc eqs

and translate_block env m subst call_stack b =
  match b with
    | BEqs (eqs, vds) ->
        let vds = List.map (fun vd -> { vd with v_ty = simplify_ty m vd.v_ty }) vds in
        let env = build_env env vds in
        translate_eqs env m subst call_stack ([], vds) eqs
    | BIf(se, trueb, elseb) ->
        if expect_bool m se then
          translate_block env m subst call_stack trueb
        else
          translate_block env m subst call_stack elseb

let node m n =
  (*Init state*)
  let call_stack = [(n.n_name, [])] in
  (*Do the translation*)
  let env = build_env IdentEnv.empty n.n_inputs in
  let env = build_env env n.n_outputs in
  let eqs, vds = translate_block env m IdentEnv.empty call_stack n.n_body in
    { n with n_body = BEqs (eqs, vds) }

let build_cd env cd =
  NameEnv.add cd.c_name cd.c_value env

let program p =
  nodes_list := p.p_nodes;
  let m = List.fold_left build_cd NameEnv.empty p.p_consts in
  if !Cli_options.no_inline_all then (
    (* Find the nodes without static parameters *)
    let nodes = List.filter (fun n -> Misc.is_empty n.n_params) p.p_nodes in
    let nodes = List.map (fun n -> node m n) nodes in
    { p with p_nodes = nodes }
  ) else (
    try
      let n = List.find (fun n -> n.n_name = !Cli_options.main_node) p.p_nodes in
      if n.n_params <> [] then (
        Format.eprintf "The main node '%s' cannot have static parameters@." n.n_name;
        raise Error
      );
      { p with p_nodes = [node m n] }
    with Not_found ->
      Format.eprintf "Cannot find the main node '%s'@." !Cli_options.main_node;
      raise Error
  )
