
let compile filename =
  try
    let p = Netlist.read_file filename in
    Netlist_printer.print_program stdout p
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    []
    compile
    ""
;;

main ()
