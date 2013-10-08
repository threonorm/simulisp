open Igraph

let rec check l = match l with
  | [] | [_] -> true
  | s1::s2::l -> (String.length s1 <= String.length s2) && (check (s2::l))

let test_good () =
  let g = mk_graph () in
  add_node g "1"; add_node g "21"; add_node g "22"; add_node g "333";
  add_edge g "1" "21"; add_edge g "1" "22";
  add_edge g "21" "333"; add_edge g "22" "333";
  let l = topological g in
  print_string "Test: Tri topologique --> ";
  if check l then print_endline "OK" else print_endline "FAIL";
  List.iter print_endline l;
  print_newline ()

let test_cycle () =
  let g = mk_graph () in
  add_node g "1"; add_node g "2"; add_node g "3";
  add_edge g "1" "2"; add_edge g "2" "3"; add_edge g "3" "1";
  print_string "Test: Detection de cycle --> ";
  if has_cycle g then print_endline "OK" else print_endline "FAIL"
;;

test_good ();;
test_cycle ();;
