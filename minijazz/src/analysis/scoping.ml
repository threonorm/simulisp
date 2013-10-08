open Ast
open Mapfold
open Static
open Static_utils
open Location
open Errors

(** Simplifies static expression in the program. *)
let simplify_program p =
  let const_dec funs cenv cd =
    let v = subst cenv cd.c_value in
    let cenv = NameEnv.add cd.c_name v cenv in
    { cd with c_value = v }, cenv
  in
  let static_exp funs cenv se =
    let se = subst cenv se in
    (match se.se_desc with
      | SVar id ->
          (* Constants with se.se_loc = no_location are generated and should not be checked *)
          if not (NameEnv.mem id cenv) && not (se.se_loc == no_location) then (
            Format.eprintf "%aThe constant name '%s' is unbound@."
              print_location se.se_loc id;
            raise Error
          )
      | _ -> ()
    );
    se, cenv
  in
  let node_dec funs cenv nd =
    let cenv' =
      List.fold_left
        (fun cenv p -> NameEnv.add p.p_name (mk_static_var p.p_name) cenv)
        cenv nd.n_params
    in
    let nd, _ = Mapfold.node_dec funs cenv' nd in
    nd, cenv
  in
  let funs =
    { Mapfold.defaults with const_dec = const_dec;
      static_exp = static_exp; node_dec = node_dec }
  in
  let p, _ = Mapfold.program_it funs NameEnv.empty p in
  p

(** Checks the name used in the program are defined.
    Adds var_decs for all variables defined in a block. *)
let check_names p =
  let rec pat_vars s pat = match pat with
    | Evarpat id -> IdentSet.add id s
    | Etuplepat ids -> List.fold_left (fun s id -> IdentSet.add id s) s ids
  in
  let build_set vds =
    List.fold_left (fun s vd -> IdentSet.add vd.v_ident s) IdentSet.empty vds
  in
  let block funs (s, _) b = match b with
    | BEqs(eqs, _) ->
        let defnames = List.fold_left (fun s (pat, _) -> pat_vars s pat) IdentSet.empty eqs in
        let ls = IdentSet.diff defnames s in (* remove outputs from the set *)
        let vds = IdentSet.fold (fun id l -> (mk_var_dec id invalid_type)::l) ls [] in
        let new_s = IdentSet.union s defnames in
        let eqs,_ = Misc.mapfold (Mapfold.equation_it funs) (new_s, IdentSet.empty) eqs in
        BEqs (eqs, vds), (s, defnames)
    | BIf(se, trueb, falseb) ->
        let trueb, (_, def_true) = Mapfold.block_it funs (s, IdentSet.empty) trueb in
        let falseb, (_, def_false) = Mapfold.block_it funs (s, IdentSet.empty) falseb in
        let defnames = IdentSet.inter def_true def_false in
        BIf(se, trueb, falseb), (s, defnames)
  in
  let exp funs (s, defnames) e = match e.e_desc with
    | Evar id ->
        if not (IdentSet.mem id s) then (
          Format.eprintf "%aThe identifier '%a' is unbound@."
            print_location e.e_loc Ident.print_ident id;
          raise Error
        );
        e, (s, defnames)
    | _ -> Mapfold.exp funs (s, defnames) e
  in
  let node n =
    let funs = { Mapfold.defaults with block = block; exp = exp } in
    let s = build_set (n.n_inputs@n.n_outputs) in
    let n_body, (_, defnames) = Mapfold.block_it funs (s, IdentSet.empty) n.n_body in
    (* check for undefined outputs *)
    let undefined_outputs =
      List.filter (fun vd -> not (IdentSet.mem vd.v_ident defnames)) n.n_outputs
    in
    if undefined_outputs <> [] then (
      Format.eprintf "%aThe following outputs are not defined: %a@."
        print_location n.n_loc  Printer.print_var_decs undefined_outputs;
      raise Error
    );
    { n with n_body = n_body }
  in
  { p with p_nodes = List.map node p.p_nodes }


let program p =
  let p = simplify_program p in
  check_names p
