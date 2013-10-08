
(* Functions to decompose a list into a tuple *)
exception Arity_error of int * int (*expected, found*)
exception Arity_min_error of int * int (*expected, found*)

let try_1 = function
  | [v] -> v
  | l -> raise (Arity_error(1, List.length l))

let try_2 = function
  | [v1; v2] -> v1, v2
  | l -> raise (Arity_error(2, List.length l))

let try_3 = function
  | [v1; v2; v3] -> v1, v2, v3
  | l -> raise (Arity_error(3, List.length l))

let try_4 = function
  | [v1; v2; v3; v4] -> v1, v2, v3, v4
  | l -> raise (Arity_error(4, List.length l))

let try_1min = function
  | v::l -> v, l
  | l -> raise (Arity_min_error(1, List.length l))

let assert_fun f l =
  try
    f l
  with
      Arity_error(expected, found) ->
        Format.eprintf "Internal compiler error: \
     wrong list size (found %d, expected %d).@." found expected;
        assert false

let assert_min_fun f l =
  try
    f l
  with
      Arity_min_error(expected, found) ->
        Format.eprintf "Internal compiler error: \
     wrong list size (found %d, expected %d at least).@." found expected;
        assert false

let assert_1 l = assert_fun try_1 l
let assert_2 l = assert_fun try_2 l
let assert_3 l = assert_fun try_3 l
let assert_4 l = assert_fun try_4 l

let assert_1min l = assert_min_fun try_1min l

let mapfold f acc l =
  let l,acc = List.fold_left
                (fun (l,acc) e -> let e,acc = f acc e in e::l, acc)
                ([],acc) l in
  List.rev l, acc

let mapi f l =
  let rec aux i = function
    | [] -> []
    | v::l -> (f i v)::(aux (i+1) l)
  in
    aux 1 l

let unique l =
  let tbl = Hashtbl.create (List.length l) in
  List.iter (fun i -> Hashtbl.replace tbl i ()) l;
  Hashtbl.fold (fun key _ accu -> key :: accu) tbl []

let is_empty = function | [] -> true | _ -> false

let gen_symbol =
  let counter = ref 0 in
  let _gen_symbol () =
    counter := !counter + 1;
    "_"^(string_of_int !counter)
  in
    _gen_symbol

let bool_of_string s = match s with
  | "t" | "1" -> true
  | "f" | "0" -> false
  | _ -> raise (Invalid_argument ("bool_of_string"))

let bool_array_of_string s =
  let a = Array.make (String.length s) false in
  for i = 0 to String.length s - 1 do
    a.(i) <- bool_of_string (String.sub s i 1)
  done;
  a

exception Int_too_big
let convert_size s n =
  let m = String.length s in
  if m > n then
    raise Int_too_big
  else
    if m = n then s else (String.make (n - m) '0')^s

let binary_not s =
  for i=0 to String.length s - 1 do
    s.[i] <- if s.[i] = '0' then '1' else '0'
  done;
  s

let rec binary_string_of_int i n =
  let rec s_of_i i = match i with
    | 0 -> "0"
    | 1 -> "1"
    | i when i < 0 -> binary_not (binary_string_of_int (-i-1) n)
    | _ ->
      let q, r = i / 2, i mod 2 in
      (s_of_i q) ^ (s_of_i r)
  in
  convert_size (s_of_i i) n

