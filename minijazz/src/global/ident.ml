
type t = {
  i_id : int;
  i_name : string;
  i_from_source : bool
}

let string_of_ident id =
  if id.i_from_source then
    id.i_name
  else
    id.i_name^"_"^(string_of_int id.i_id)

let print_ident ff id =
  Format.fprintf ff "%s" (string_of_ident id)

module StringEnv = Map.Make (struct type t = string let compare = compare end)

let ident_counter = ref 0
let fresh_ident from_source s =
  incr ident_counter;
  { i_id = !ident_counter; i_name = s; i_from_source = from_source }

let copy id =
  fresh_ident false (string_of_ident id)

let symbol_table = ref StringEnv.empty
let reset_symbol_table () =
  symbol_table := StringEnv.empty
let ident_of_string s =
  if StringEnv.mem s !symbol_table then
    StringEnv.find s !symbol_table
  else (
    let id = fresh_ident true s in
    symbol_table := StringEnv.add s id !symbol_table;
    id
  )

let fresh_ident = fresh_ident false
