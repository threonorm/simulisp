open Ast
open Static

let is_not_zero ty = match ty with
  | TBitArray { se_desc = SInt 0 } -> false
  | _ -> true

let rec simplify_exp e = match e.e_desc with
  (* replace x[i..j] with [] if j < i *)
  | Ecall("slice",
         [{ se_desc = SInt min };
          { se_desc = SInt max }; n], _) when max < min ->
      { e with e_desc = Econst (VBitArray (Array.make 0 false)) }
  (* replace x[i..i] with x[i] *)
  | Ecall("slice", [min; max; n], args) when min = max ->
      let new_e = { e with e_desc = Ecall("select", [min; n], args) } in
      simplify_exp new_e
  (* replace x.[] or [].x with x *)
  | Ecall("concat", _, [{ e_ty = TBitArray { se_desc = SInt 0 } }; e1])
  | Ecall("concat", _, [e1; { e_ty = TBitArray { se_desc = SInt 0 } }]) ->
      e1
  | Ecall(f, params, args) ->
      { e with e_desc = Ecall(f, params, List.map simplify_exp args) }
  | _ -> e

let simplify_eq (pat,e) =
  (pat, simplify_exp e)

let rec block b = match b with
  | BEqs(eqs, vds) ->
      let eqs = List.map simplify_eq eqs in
      (* remove variables with size 0 *)
      let vds = List.filter (fun vd -> is_not_zero vd.v_ty) vds in
      let eqs = List.filter (fun (_, e) -> is_not_zero e.e_ty) eqs in
      BEqs(eqs, vds)
  | BIf(se, trueb, elseb) -> BIf(se, block trueb, block elseb)

let node n =
  { n with n_body = block n.n_body }

let program p =
    { p with p_nodes = List.map node p.p_nodes }
