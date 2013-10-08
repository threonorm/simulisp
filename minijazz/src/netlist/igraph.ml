(*open Graph

module G =
  Imperative.Digraph.Abstract(struct
    type t = string
  end)

module DepTopological = Topological.Make(G)
module DepTraverse = Traverse.Mark(G)

module IdentEnv = Map.Make (struct type t = string let compare = compare end)

(* on ne peut pas faire de graphe polymorphe avec ocamlgraph
   a cause des limitations du systeme de module de OCaml *)
type graph =
    { g_graph : G.t;
      mutable g_env : G.V.t IdentEnv.t }

let mk_graph () =
  { g_graph = G.create ();
    g_env = IdentEnv.empty }

let add_node g id =
  let n = G.V.create id in
  G.add_vertex g.g_graph n;
  g.g_env <- IdentEnv.add id n g.g_env

let node_for_label g x =
  IdentEnv.find x g.g_env

let add_edge g id1 id2 =
  let n1 = node_for_ident g id1 in
  let n2 = node_for_ident g id2 in
  G.add_edge g.g_graph n1 n2

let topological g =
  let l = DepTopological.fold (fun v acc -> (G.V.label v) :: acc) g.g_graph [] in
  List.rev l

let has_cycle g =
  DepTraverse.has_cycle g.g_graph
*)

exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  let rec visit n = match n.n_mark with
    | NotVisited ->
        n.n_mark <- InProgress;
        List.iter visit n.n_link_to;
        n.n_mark <- Visited
    | InProgress -> raise Cycle
    | Visited -> ()
  in
  try
    clear_marks g;
    List.iter visit g.g_nodes; false
  with
    | Cycle -> true


let topological g =
  let rec visit acc n = match n.n_mark with
    | NotVisited ->
        n.n_mark <- InProgress;
        let l = List.fold_left visit [] n.n_link_to in
        n.n_mark <- Visited;
        n.n_label::(l@acc)
    | InProgress -> raise Cycle
    | Visited -> acc
  in
  try
    clear_marks g;
    List.fold_left visit [] (find_roots g)
  with
    | Cycle -> []
