{
open Lexing
open Netlist_parser
exception Eof

let keyword_list =
[
  "AND", AND;
  "CONCAT", CONCAT;
  "IN", IN;
  "INPUT", INPUT;
  "MUX", MUX;
  "NAND", NAND;
  "NOT", NOT;
  "OR", OR;
  "OUTPUT", OUTPUT;
  "RAM", RAM;
  "REG", REG;
  "ROM", ROM;
  "SELECT", SELECT;
  "SLICE", SLICE;
  "VAR", VAR;
  "XOR", XOR;
]

}

rule token = parse
    '\n'
      { new_line lexbuf; token lexbuf }     (* skip blanks *)
  | [' ' '\t']
      { token lexbuf }     (* skip blanks *)
  | "="            { EQUAL }
  | ":"            { COLON }
  | ","            { COMMA }
  | ['0'-'9']+ as lxm { CONST lxm }
  | ('_' ? ['A'-'Z' 'a'-'z']('_' ? ['A'-'Z' 'a'-'z' ''' '0'-'9']) * as id)
      { let s = Lexing.lexeme lexbuf in
        try List.assoc s keyword_list
        with Not_found -> NAME id }
  | eof            { EOF }
