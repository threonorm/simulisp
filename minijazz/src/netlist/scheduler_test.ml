let print_only = ref false
let number_steps = ref (-1)

let compile filename =
  try
    let p = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    begin try
        let p = Scheduler.schedule p in
        Netlist_printer.print_program out p;
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
            close_all (); exit 2
    end;
    close_all ();
    if not !print_only then (
      let simulator =
        if !number_steps = -1 then
          "./netlist_simulator.byte"
        else
          "./netlist_simulator.byte -n "^(string_of_int !number_steps)
      in
      ignore (Unix.system (simulator^" "^out_name))
    )
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
