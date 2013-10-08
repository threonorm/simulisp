open Netlist_ast
open Igraph

exception Combinational_cycle

let assert_1min l = match l with
  | v::l -> v, l
  | l -> assert false

let add id acc =
  if List.mem id acc then acc else id::acc

let read_arg acc a = match a with
  | Aconst _ -> acc
  | Avar id -> add id acc
(** @return the variables read by an expression
    (variables to the right of a register are ignored)*)
let rec read acc e = match e with
  (* no dependency for registers *)
  | Ereg _ -> acc
  (* for a RAM, we only depend on the read_addr *)
  | Eram(_, _, read_addr, _, _, _) -> read_arg acc read_addr
  | Earg a | Enot a | Eselect (_, a) | Eslice (_, _, a) | Erom (_, _, a) -> read_arg acc a
  | Ebinop (_, a1, a2) | Econcat (a1, a2) -> read_arg (read_arg acc a2) a1
  | Emux(a1, a2, a3) -> read_arg (read_arg (read_arg acc a3) a2) a1

let read (_, e) = read [] e
let def (x, _) = [x]

let mk_dependency_graph p =
  let add_depends g eq =
    let attach (n1, _) n2 =
      try
        add_edge g n1 n2
      with
        | Not_found -> () (*n is an input, no dependency*)
    in
    List.iter (attach eq) (read eq)
  in
  (** Creates the initial graph (one node for each equation)
      and an environment mapping idents to nodes. *)
  let g = mk_graph () in
  List.iter (fun (x, _) -> add_node g x) p.p_eqs;
  (** Add the dependences corresponding to the equation in the graph. *)
  List.iter (add_depends g) p.p_eqs;
  g

let schedule p =
  let g = mk_dependency_graph p in
  if has_cycle g then
    raise Combinational_cycle;
  let ids = List.rev (topological g) in
  let eqs = List.map (fun id -> List.find (fun (id2, _) -> id = id2) p.p_eqs) ids in
  { p with p_eqs = eqs }
