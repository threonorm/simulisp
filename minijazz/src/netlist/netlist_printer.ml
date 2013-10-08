open Netlist_ast
open Format

let rec print_env print lp sep rp ff env =
  let first = ref true in
  fprintf ff "%s" lp;
  Env.iter
    (fun x ty ->
      if !first then
        (first := false; fprintf ff "%a" print (x, ty))
      else
        fprintf ff "%s%a" sep print (x, ty)) env;
  fprintf ff "%s" rp

let rec print_list print lp sep rp ff = function
  | [] -> ()
  | x :: l ->
      fprintf ff "%s%a" lp print x;
      List.iter (fprintf ff "%s %a" sep print) l;
      fprintf ff "%s" rp

let print_ty ff ty = match ty with
  | TBit -> ()
  | TBitArray n -> fprintf ff " : %d" n

let print_bool ff b =
  if b then
    fprintf ff "1"
  else
    fprintf ff "0"

let print_value ff v = match v with
  | VBit b -> print_bool ff b
  | VBitArray a -> Array.iter (print_bool ff) a

let print_arg ff arg = match arg with
  | Aconst v -> print_value ff v
  | Avar id -> fprintf ff "%s" id

let print_op ff op = match op with
  | And -> fprintf ff "AND"
  | Nand -> fprintf ff "NAND"
  | Or -> fprintf ff "OR"
  | Xor -> fprintf ff "XOR"

let print_exp ff e = match e with
  | Earg a -> print_arg ff a
  | Ereg x -> fprintf ff "REG %s" x
  | Enot x -> fprintf ff "NOT %a" print_arg x
  | Ebinop(op, x, y) -> fprintf ff  "%a %a %a" print_op op  print_arg x  print_arg y
  | Emux (c, x, y) -> fprintf ff "MUX %a %a %a " print_arg c  print_arg x  print_arg y
  | Erom (addr, word, ra) -> fprintf ff "ROM %d %d %a" addr word  print_arg ra
  | Eram (addr, word, ra, we, wa, data) ->
      fprintf ff "RAM %d %d %a %a %a %a" addr word
        print_arg ra  print_arg we
        print_arg wa  print_arg data
  | Eselect (idx, x) -> fprintf ff "SELECT %d %a" idx print_arg x
  | Econcat (x, y) ->  fprintf ff  "CONCAT %a %a" print_arg x  print_arg y
  | Eslice (min, max, x) -> fprintf ff "SLICE %d %d %a" min max print_arg x

let print_eq ff (x, e) =
  fprintf ff "%s = %a@." x print_exp e

let print_var ff (x, ty) =
  fprintf ff "@[%s%a@]" x print_ty ty

let print_vars ff env =
  fprintf ff "@[<v 2>VAR@,%a@]@.IN@,"
    (print_env print_var "" ", " "") env

let print_idents ff ids =
  let print_ident ff s = fprintf ff "%s" s in
  print_list print_ident """,""" ff ids

let print_program oc p =
  let ff = formatter_of_out_channel oc in
  fprintf ff "INPUT %a@." print_idents p.p_inputs;
  fprintf ff "OUTPUT %a@." print_idents p.p_outputs;
  print_vars ff p.p_vars;
  List.iter (print_eq ff) p.p_eqs;
  (* flush *)
  fprintf ff "@."
