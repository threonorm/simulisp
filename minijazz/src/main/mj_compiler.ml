open Errors
open Cli_options
open Location

let separateur = "\n*********************************************\
    *********************************\n*** "

let comment ?(sep=separateur) s =
  if !verbose then Format.printf "%s%s@." sep s

let do_pass d f p pp =
  comment (d^" ...\n");
  let r = f p in
  if !verbose then pp r;
  comment ~sep:"*** " (d^" done.");
  r

let do_silent_pass d f p = do_pass d f p (fun _ -> ())

let pass d enabled f p pp =
  if enabled
  then do_pass d f p pp
  else p

let silent_pass d enabled f p =
  if enabled
  then do_silent_pass d f p
  else p

let parse lexbuf =
  try
    Parser.program Lexer.token lexbuf
  with
    | Lexer.Lexical_error(err, l) ->
        lexical_error err l
    | Parser.Error ->
        let pos1 = Lexing.lexeme_start_p lexbuf
        and pos2 = Lexing.lexeme_end_p lexbuf in
        let l = Loc(pos1,pos2) in
        syntax_error l

let lexbuf_from_file file_name =
  let ic = open_in file_name in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
      { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file_name };
  ic, lexbuf

let compile_impl filename =
  (* input and output files *)
  let ic, lexbuf = lexbuf_from_file filename in
  let net_name = (Filename.chop_suffix filename ".mj") ^ ".net" in
  let net = open_out net_name in
  let close_all_files () =
    close_in ic;
    close_out net
  in
  try
    base_path := Filename.dirname filename;

    let pp = Printer.print_program stdout in
    (* Parsing of the file *)
    let p = do_pass "Parsing" parse lexbuf pp in

    let p = pass "Scoping" true Scoping.program p pp in

    let p = pass "Typing" true Typing.program p pp in

    let p = pass "Normalize" true Normalize.program p pp in

    let p = pass "Callgraph" true Callgraph.program p pp in

    let p = pass "Simplify" true Simplify.program p pp in

    let p = Mj2net.program p in
    Netlist_printer.print_program net p;

    close_all_files ()
  with
    | x -> close_all_files (); raise x
