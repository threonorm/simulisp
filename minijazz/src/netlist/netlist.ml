open Lexing

exception Parse_error of string

let find_file filename =
  try
    open_in filename
  with
    | _ -> raise (Parse_error ("No such file '"^filename^"%s'"))

(** [read_file filename] reads the [filename] file and outputs the corresponding
    Netlist_ast.program.*)
let read_file filename =
  let ic = find_file filename in
  let lexbuf = from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    Netlist_parser.program Netlist_lexer.token lexbuf
  with
    | e ->
        let loc = Format.sprintf "line %d, column %d"
          lexbuf.lex_curr_p.pos_lnum
          (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
        in
        raise (Parse_error ("Syntax error at "^loc))

(** [print_program oc p] prints the program [p] on the output channel [oc].
    For instance, to print on the standard output, use [print_program stdout p].
    To print to a file named 'filename', use the following:
        let out = open_out filename in
        print_program out p
*)
let print_program = Netlist_printer.print_program
