open Ast
open Static
open Static_utils
open Printer
open Errors
open Misc
open Mapfold

exception Unify

type error_kind =
  | Args_arity_error of int * int
  | Params_arity_error of int * int
  | Result_arity_error of int * int
  | Type_error of ty * ty
  | Static_type_error of static_ty * static_ty
  | Static_constraint_false of static_exp

exception Typing_error of error_kind

let error k = raise (Typing_error k)

let message loc err = match err with
  | Args_arity_error (found, expected) ->
    Format.eprintf "%aWrong number of arguments (found '%d'; expected '%d')@."
      Location.print_location loc  found expected
  | Result_arity_error (found, expected) ->
    Format.eprintf "%aWrong number of outputs (found '%d'; expected '%d')@."
      Location.print_location loc  found expected
  | Params_arity_error (found, expected) ->
    Format.eprintf "%aWrong number of static parameters (found '%d'; expected '%d')@."
      Location.print_location loc  found expected
  | Type_error (found_ty, exp_ty) ->
    Format.eprintf "%aThis expression has type '%a' but '%a' was expected@."
      Location.print_location loc  print_type found_ty  print_type exp_ty
  | Static_type_error (found_ty, exp_ty) ->
    Format.eprintf "%aThis static expression has type '%a' but '%a' was expected@."
      Location.print_location loc  print_static_type found_ty print_static_type exp_ty
  | Static_constraint_false se ->
    Format.eprintf "%aThe following constraint is not satisfied: %a@."
      Location.print_location loc  print_static_exp se


type signature =
    { s_inputs : ty list;
      s_outputs : ty list;
      s_params : name list;
      s_constraints : static_exp list }

module Modules = struct
  let env = ref Ast.NameEnv.empty

  let add_sig ?(params = []) ?(constr = []) n inp outp =
    let s = { s_inputs = inp; s_outputs = outp; s_params = params; s_constraints = constr } in
    env := Ast.NameEnv.add n s !env

  let _ =
    add_sig "and" [TBit;TBit] [TBit];
    add_sig "xor" [TBit;TBit] [TBit];
    add_sig "or"  [TBit;TBit] [TBit];
    add_sig "not" [TBit] [TBit];
    add_sig "reg" [TBit] [TBit];
    add_sig "mux" [TBit;TBit;TBit] [TBit];
    add_sig ~params:["n"] "print" [TBitArray (mk_static_var "n"); TBit] [TBit];
    add_sig ~params:["n"] "input" [TBit] [TBitArray (mk_static_var "n")];
    let constr1 = mk_static_exp (SBinOp(SLess, mk_static_var "i", mk_static_var "n")) in
    let constr2 = mk_static_exp (SBinOp(SLeq, mk_static_int 0, mk_static_var "i")) in
    add_sig ~params:["i"; "n"]
      ~constr:[constr1; constr2]
      "select" [TBitArray (mk_static_var "n")] [TBit];
    let add = mk_static_exp (SBinOp(SAdd, mk_static_var "n1", mk_static_var "n2")) in
    add_sig ~params:["n1"; "n2"; "n3"]
      ~constr:[mk_static_exp (SBinOp (SEqual, mk_static_var "n3", add))]
      "concat" [TBitArray (mk_static_var "n1"); TBitArray (mk_static_var "n2")]
      [TBitArray (mk_static_var "n3")];
    (* slice :  size = min <= max ? max - min + 1 : 0 *)
    let size =
      mk_static_exp
        (SBinOp(SAdd,
               mk_static_exp (SBinOp(SMinus, mk_static_var "max", mk_static_var "min")),
               mk_static_int 1))
    in
    let size =
      mk_static_exp (SIf (mk_static_exp (SBinOp(SLeq, mk_static_var "min", mk_static_var "max")),
                                        size, mk_static_int 0))
    in
    let constr1 = mk_static_exp (SBinOp(SLeq, mk_static_int 0, mk_static_var "min")) in
    let constr2 = mk_static_exp (SBinOp(SLess, mk_static_var "max", mk_static_var "n")) in
    add_sig ~params:["min"; "max"; "n"] ~constr:[constr1; constr2] "slice"
      [TBitArray (mk_static_var "n")] [TBitArray size]


  let tys_of_vds vds = List.map (fun vd -> vd.v_ty) vds

  let add_node n constr =
    let s = { s_inputs = tys_of_vds n.n_inputs;
              s_outputs = tys_of_vds n.n_outputs;
              s_params = List.map (fun p -> p.p_name) n.n_params;
              s_constraints = constr } in
    env := Ast.NameEnv.add n.n_name s !env

  let build_param_env param_names params =
    List.fold_left2
      (fun env pn p -> NameEnv.add pn p env)
      NameEnv.empty param_names params

  let subst_ty env ty = match ty with
    | TBitArray se -> TBitArray (subst env se)
    | _ -> ty

  let find_node n params =
    try
      let s = Ast.NameEnv.find n !env in
      if List.length s.s_params <> List.length params then
        error (Params_arity_error (List.length params, List.length s.s_params));
      let env = build_param_env s.s_params params in
      let s =
        { s with s_inputs = List.map (subst_ty env) s.s_inputs;
          s_outputs = List.map (subst_ty env) s.s_outputs;
          s_constraints = List.map (subst env) s.s_constraints }
      in
      s
    with Not_found ->
      Format.eprintf "Unbound node '%s'@." n;
      raise Error
end

let constr_list = ref []
let add_constraint se =
  constr_list := se :: !constr_list
let set_constraints cl =
  constr_list := cl
let get_constraints () =
  let v = !constr_list in
  constr_list := []; v

let fresh_static_var () =
  SVar ("s_"^(Misc.gen_symbol ()))

(* Functions on types*)

let fresh_type =
  let index = ref 0 in
  let gen_index () = (incr index; !index) in
  let fresh_type () = TVar (ref (TIndex (gen_index ()))) in
  fresh_type

(** returns the canonic (short) representant of [ty]
    and update it to this value. *)
let rec ty_repr ty = match ty with
  | TVar link ->
    (match !link with
      | TLink ty ->
        let ty = ty_repr ty in
        link := TLink ty;
        ty
      | _ -> ty)
  | _ -> ty

(** verifies that index is fresh in ck. *)
let rec occur_check index ty =
  let ty = ty_repr ty in
  match ty with
    | TUnit | TBit | TBitArray _  -> ()
    | TVar { contents = TIndex n } when index <> n -> ()
    | TProd ty_list -> List.iter (occur_check index) ty_list
    | _ -> raise Unify

let rec unify ty1 ty2 =
  let ty1 = ty_repr ty1 in
  let ty2 = ty_repr ty2 in
  if ty1 == ty2 then ()
  else
   match (ty1, ty2) with
     | TBitArray n, TBit | TBit, TBitArray n ->
         add_constraint (mk_static_exp (SBinOp(SEqual, n, mk_static_int 1)))
     | TBitArray n1, TBitArray n2 ->
         add_constraint (mk_static_exp (SBinOp(SEqual, n1, n2)))
     | TVar { contents = TIndex n1 }, TVar { contents = TIndex n2 } when n1 = n2 -> ()
     | TProd ty_list1, TProd ty_list2 ->
       if List.length ty_list1 <> List.length ty_list2 then
         error (Result_arity_error (List.length ty_list1, List.length ty_list2));
       List.iter2 unify ty_list1 ty_list2
     | TVar ({ contents = TIndex n } as link), ty
     | ty, TVar ({ contents = TIndex n } as link) ->
       occur_check n ty;
       link := TLink ty
     | _ -> raise Unify

let prod ty_list = match ty_list with
  | [ty] -> ty
  | _ -> TProd ty_list

(* Typing of static exps *)
let rec type_static_exp se = match se.se_desc with
    | SInt _ | SVar _ -> STInt
    | SBool _ -> STBool
    | SBinOp((SAdd | SMinus | SMult | SDiv | SPower ), se1, se2) ->
      expect_static_exp se1 STInt;
      expect_static_exp se2 STInt;
      STInt
    | SBinOp((SEqual | SLess | SLeq | SGreater | SGeq), se1, se2) ->
      expect_static_exp se1 STInt;
      expect_static_exp se2 STInt;
      STBool
    | SIf (c, se1, se2) ->
        expect_static_exp se1 STBool;
        let ty1 = type_static_exp se1 in
        expect_static_exp se2 ty1;
        ty1

and expect_static_exp se ty =
  let found_ty = type_static_exp se in
  if found_ty <> ty then
    error (Static_type_error (found_ty, ty))

let rec simplify_constr cl = match cl with
  | [] -> []
  | c::cl ->
      let c' = simplify NameEnv.empty c in
      match c'.se_desc with
        | SBool true -> simplify_constr cl
        | SBool false -> error (Static_constraint_false c)
        | _ -> c::(simplify_constr cl)

let rec find_simplification_one c = match c.se_desc with
  | SBinOp(SEqual, { se_desc = SVar s }, se)
  | SBinOp(SEqual, se, { se_desc = SVar s }) ->
      Some (s, se)
  | SIf(_, se1, { se_desc = SBool true })
  | SIf(_, { se_desc = SBool true }, se1) ->
      find_simplification_one se1
  | _ -> None

let rec find_simplification params cl = match cl with
  | [] -> None, []
  | c::cl ->
      (match find_simplification_one c with
        | Some (s, se) when not (List.mem s params) ->
            Some (s, se), cl
        | _ ->
            let res, cl = find_simplification params cl in
            res, c::cl)

let solve_constr params cl =
  let params = List.map (fun p -> p.p_name) params in
  let subst_and_error env c =
    let c' = subst env c in
    match c'.se_desc with
      | SBool false -> error (Static_constraint_false c)
      | _ -> c'
  in
  let env = ref NameEnv.empty in
  let rec solve_one cl =
    let res, cl = find_simplification params cl in
    match res with
      | None -> cl
      | Some (s, se) ->
          env := NameEnv.add s se !env;
          let cl = List.map (subst_and_error !env) cl in
          solve_one cl
  in
  let cl = simplify_constr cl in
  let cl = solve_one cl in
  cl, !env

(* Typing of expressions *)
let rec type_exp env e =
  try
    let desc, ty = match e.e_desc with
      | Econst (VBit _) -> e.e_desc, TBit
      | Econst (VBitArray a) -> e.e_desc, TBitArray (mk_static_int (Array.length a))
      | Evar id -> Evar id, IdentEnv.find id env
      | Ereg e ->
          let e = expect_exp env e TBit in
          Ereg e, TBit
      | Emem (MRom, addr_size, word_size, file, args) ->
          (* addr_size > 0 *)
          add_constraint (mk_static_exp (SBinOp (SLess, mk_static_int 0, addr_size)));
          let read_addr = assert_1 args in
          let read_addr = expect_exp env read_addr (TBitArray addr_size) in
          Emem (MRom, addr_size, word_size, file, [read_addr]), TBitArray word_size
      | Emem (MRam, addr_size, word_size, file, args) ->
          (* addr_size > 0 *)
          add_constraint (mk_static_exp (SBinOp (SLess, mk_static_int 0, addr_size)));
          let read_addr, write_en, write_addr, data_in = assert_4 args in
          let read_addr = expect_exp env read_addr (TBitArray addr_size) in
          let write_addr = expect_exp env write_addr (TBitArray addr_size) in
          let data_in = expect_exp env data_in (TBitArray word_size) in
          let write_en = expect_exp env write_en TBit in
          let args = [read_addr; write_en; write_addr; data_in] in
          Emem (MRam, addr_size, word_size, file, args), TBitArray word_size
      | Ecall (f, params, args) ->
          let s = Modules.find_node f params in
          (*check arity*)
          if List.length s.s_inputs <> List.length args then
            error (Args_arity_error (List.length args, List.length s.s_inputs));
          (*check types of all arguments*)
          let args = List.map2 (expect_exp env) args s.s_inputs in
          List.iter add_constraint s.s_constraints;
          Ecall(f, params, args), prod s.s_outputs
    in
    { e with e_desc = desc; e_ty = ty }, ty
  with
    | Typing_error k -> message e.e_loc k; raise Error

and expect_exp env e ty =
  let e, found_ty = type_exp env e in
    try
      unify ty found_ty;
      e
    with
        Unify -> error (Type_error (found_ty, ty))

let type_pat env pat = match pat with
  | Evarpat x -> IdentEnv.find x env
  | Etuplepat id_list -> prod (List.map (fun x -> IdentEnv.find x env) id_list)

let type_eq env (pat, e) =
  let pat_ty = type_pat env pat in
  let e = expect_exp env e pat_ty in
    (pat, e)

let build env vds =
  let build_one env vd = IdentEnv.add vd.v_ident vd.v_ty env in
    List.fold_left build_one env vds

let rec type_block env b = match b with
  | BEqs(eqs, vds) ->
    let vds = List.map (fun vd -> { vd with v_ty = fresh_type () }) vds in
    let env = build env vds in
    let eqs = List.map (type_eq env) eqs in
    BEqs(eqs,vds)
  | BIf(se, trueb, falseb) ->
      expect_static_exp se STBool;
      let prev_constr = get_constraints () in
      let trueb = type_block env trueb in
      let true_constr =
        List.map (fun c -> mk_static_exp (SIf (se, c, mk_static_bool true))) (get_constraints ())
      in
      let falseb = type_block env falseb in
      let false_constr =
        List.map (fun c -> mk_static_exp (SIf (se, mk_static_bool true, c))) (get_constraints ())
      in
      set_constraints (prev_constr @ true_constr @ false_constr);
      BIf(se, trueb, falseb)

let ty_repr_block env b =
  let static_exp funs acc se = simplify env se, acc in
  let ty funs acc ty =
    let ty = ty_repr ty in
    (* go through types to substitute static exps *)
    Mapfold.ty funs acc ty
  in
  let funs = { Mapfold.defaults with ty = ty; static_exp = static_exp } in
  let b, _ = Mapfold.block_it funs () b in
  b

let node n =
  try
    Modules.add_node n [];
    let env = build IdentEnv.empty n.n_inputs in
    let env = build env n.n_outputs in
    let body = type_block env n.n_body in
    let constr = get_constraints () in
    let constr, env = solve_constr n.n_params constr in
    let body = ty_repr_block env body in
    Modules.add_node n constr;
    { n with n_body = body; n_constraints = constr }
  with
      Typing_error k -> message n.n_loc k; raise Error

let program p =
  let p_nodes = List.map node p.p_nodes in
    { p with p_nodes = p_nodes }
