open Ast
open Static
open Static_utils
open Ident

let expect_int se =
  let se = simplify NameEnv.empty se in
  match se.se_desc with
    | SInt v -> v
    | _ ->
        Format.eprintf "Unexpected static exp: %a@." Printer.print_static_exp se;
        assert false

let expect_ident e = match e.e_desc with
  | Evar id -> string_of_ident id
  | _ -> assert false

let tr_value v = match v with
  | VBit b -> Netlist_ast.VBit b
  | VBitArray a -> Netlist_ast.VBitArray a

let tr_ty ty = match ty with
  | TBit -> Netlist_ast.TBit
  | TBitArray se -> Netlist_ast.TBitArray (expect_int se)
  | _ -> Format.eprintf "Unexpected type: %a@." Printer.print_type ty; assert false

let tr_var_dec { v_ident = n; v_ty = ty } =
  string_of_ident n, tr_ty ty

let tr_pat pat = match pat with
  | Evarpat id -> string_of_ident id
  | Etuplepat ids ->
      Format.eprintf "Unexpected pattern: %a@." Printer.print_pat pat;
      assert false

let expect_arg e = match e.e_desc with
  | Evar id -> Netlist_ast.Avar (string_of_ident id)
  | Econst v -> Netlist_ast.Aconst (tr_value v)
  | _ -> Format.eprintf "Unexpected arg : %a@." Printer.print_exp e; assert false

let rec tr_exp e = match e.e_desc with
  | Evar id -> Netlist_ast.Earg (Netlist_ast.Avar (string_of_ident id))
  | Econst v ->  Netlist_ast.Earg (Netlist_ast.Aconst (tr_value v))
  | Ereg e -> Netlist_ast.Ereg (expect_ident e)
  | Ecall ("not", _, [e]) -> Netlist_ast.Enot (expect_arg e)
  | Ecall (("or" | "xor" | "and" | "nand") as op, _, [e1; e2]) ->
      let op =
        match op with
          | "or" -> Netlist_ast.Or
          | "xor" -> Netlist_ast.Xor
          | "and" -> Netlist_ast.And
          | "nand" -> Netlist_ast.Nand
          | _ -> assert false
      in
      Netlist_ast.Ebinop (op, expect_arg e1, expect_arg e2)
  | Ecall ("mux", _, [e1; e2; e3]) ->
      Netlist_ast.Emux (expect_arg e1, expect_arg e2, expect_arg e3)
  | Ecall("select", idx::_, [e]) ->
      Netlist_ast.Eselect (expect_int idx, expect_arg e)
  | Ecall("slice", min::max::_, [e]) ->
      Netlist_ast.Eslice (expect_int min, expect_int max, expect_arg e)
  | Ecall("concat", _, [e1; e2]) ->
      Netlist_ast.Econcat (expect_arg e1, expect_arg e2)
  | Emem(MRom, addr_size, word_size, _, [e]) ->
      Netlist_ast.Erom (expect_int addr_size, expect_int word_size, expect_arg e)
  | Emem(MRam, addr_size, word_size, _, [ra; we; wa; data]) ->
      Netlist_ast.Eram (expect_int addr_size, expect_int word_size,
                       expect_arg ra, expect_arg we, expect_arg wa, expect_arg data)
  | _ -> assert false

let tr_eq (pat, e) =
  tr_pat pat, tr_exp e

let tr_vds env vds =
  List.fold_left
    (fun env vd -> Netlist_ast.Env.add (string_of_ident vd.v_ident) (tr_ty vd.v_ty) env)
    env vds

let tr_block b = match b with
  | BEqs (eqs, vds) ->
      let env = tr_vds Netlist_ast.Env.empty vds in
      let eqs = List.map tr_eq eqs in
      env, eqs
  | _ -> assert false

let program p =
  let n = match p.p_nodes with [n] -> n | _ -> assert false in
  let vars, eqs = tr_block n.n_body in
  let vars = tr_vds vars n.n_inputs in
  let vars = tr_vds vars n.n_outputs in
  let inputs = List.map (fun vd -> string_of_ident vd.v_ident) n.n_inputs in
  let outputs = List.map (fun vd -> string_of_ident vd.v_ident) n.n_outputs in
  { Netlist_ast.p_inputs = inputs; Netlist_ast.p_outputs = outputs;
    Netlist_ast.p_vars = vars; Netlist_ast.p_eqs = eqs }
