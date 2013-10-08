(* version of the compiler *)
let version = "0.2.1"

let verbose = ref false
let print_types = ref false
let no_inline_all = ref false
let main_node = ref "main"

let base_path = ref ""

let show_version () =
  Format.printf "The MiniJazz compiler, version %s @." version
let errmsg = "Options are:"

let doc_verbose = "\t\t\tSet verbose mode"
and doc_version = "\t\tThe version of the compiler"
and doc_full_type_info = "\t\tPrint full type information"
and doc_main_node = "\t\t\tSet the main node"
