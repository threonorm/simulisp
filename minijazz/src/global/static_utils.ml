open Static

let pow a n =
  let rec g p x = function
  | 0 -> x
  | i ->
      g (p * p) (if i mod 2 = 1 then p * x else x) (i/2)
  in
  g a 1 n
;;


let fun_of_op op = match op with
  | SAdd -> (+) | SMinus -> (-)
  | SMult -> (fun i1 i2 -> i1 * i2)
  | SDiv -> (/)
  | SPower -> pow
  | _ -> assert false

let fun_of_comp_op op = match op with
  | SEqual -> (=) | SLeq -> (<=)  | SLess -> (<)
  | _ -> assert false

let rec simplify env se = match se.se_desc with
  | SInt _ | SBool _ -> se
  | SVar n ->
      (try
         let se = NameEnv.find n env in
         simplify env se
        with
          | Not_found -> se)
  | SBinOp(op, se1, se2) ->
      let se1 = simplify env se1 in
      let se2 = simplify env se2 in
      let desc =
        match op, se1.se_desc, se2.se_desc with
          | (SAdd | SMinus | SDiv  | SMult | SPower), SInt i1, SInt i2 ->
              let f = fun_of_op op in
              SInt (f i1 i2)
        | (SEqual | SLess | SLeq | SGreater | SGeq), SInt i1, SInt i2 ->
            let f = fun_of_comp_op op in
            SBool (f i1 i2)
        | _, _, _ -> SBinOp(op, se1, se2)
      in
      { se with se_desc = desc }
  | SIf(c, se1, se2) ->
      let c = simplify env c in
      let se1 = simplify env se1 in
      let se2 = simplify env se2 in
      (match c.se_desc, se1.se_desc, se2.se_desc with
        | SBool true, _, _ -> se1
        | SBool false, _, _ -> se2
        | _, sed1, sed2 when sed1 = sed2 -> { se with se_desc = sed1 }
        | _, _, _ -> { se with se_desc = SIf(c, se1, se2) })

let rec subst env se = match se.se_desc with
  | SInt _ | SBool _ -> se
  | SVar n ->
      (try
          NameEnv.find n env
        with
          | Not_found -> se)
  | SBinOp(op, se1, se2) ->
      { se with se_desc = SBinOp(op, subst env se1, subst env se2) }
  | SIf(c, se1, se2) ->
      { se with se_desc = SIf(subst env c, subst env se1, subst env se2) }

exception Unsatisfiable of static_exp
let check_true env cl =
  let check_one c =
    let res = simplify env c in
    match res.se_desc with
      | SBool true -> ()
      | _ -> raise (Unsatisfiable c)
  in
  List.iter check_one cl
