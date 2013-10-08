open Cli_options
open Mj_compiler

let main () =
  try
    Arg.parse
      [
        "-v",Arg.Set verbose, doc_verbose;
        "-version", Arg.Unit show_version, doc_version;
        "-m", Arg.Set_string main_node, doc_main_node;
        "-print-types", Arg.Set print_types, doc_full_type_info;
      ]
      compile_impl
      errmsg;
  with
    | Errors.Error -> exit 2;;

main ()


