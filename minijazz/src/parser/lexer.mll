(* lexer.mll *)


{
open Location
open Lexing
open Parser
open Errors

exception Lexical_error of lexical_error * location;;

let comment_depth = ref 0

let keyword_table = ((Hashtbl.create 149) : (string, token) Hashtbl.t);;

List.iter (fun (str,tok) -> Hashtbl.add keyword_table str tok) [
 "ram", RAM;
 "rom", ROM;
 "where", WHERE;
 "end", END;
 "true", BOOL(true);
 "false", BOOL(false);
 "reg", REG;
 "not", NOT;
 "const", CONST;
 "and", AND;
 "nand", NAND;
 "or", OR;
 "xor", XOR;
 "if", IF;
 "then", THEN;
 "else", ELSE;
 "inlined", INLINED;
 "probing", PROBING
]


(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()

(*
let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
*)

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.set (!string_buff) (!string_index) c;
  incr string_index


let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c =
    100 * (int_of_char(Lexing.lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)

}

let newline = '\n' | '\r' '\n'

rule token = parse
  | newline         { new_line lexbuf; token lexbuf }
  | [' ' '\t'] +    { token lexbuf }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "*"             { STAR }
  | "+"             { PLUS }
  | "&"             { AND }
  | "/"             { SLASH }
  | "<"             { LESS }
  | ">"             { GREATER }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ":"             { COLON }
  | ";"             { SEMICOL }
  | "="             { EQUAL }
  | ","             { COMMA }
  | "-"             { MINUS }
  | "^"             { POWER }
  | "<="            { LEQ }
  | "."             { DOT }
  | ".."            { DOTDOT }
  | (['A'-'Z']('_' ? ['A'-'Z' 'a'-'z' ''' '0'-'9']) * as id)
      {NAME id}
  | (['A'-'Z' 'a'-'z']('_' ? ['A'-'Z' 'a'-'z' ''' '0'-'9']) * as id)
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> NAME id }
  | '0' ['b' 'B'] (['0'-'1']+ as lit)
      { BOOL_INT lit }
  | ['0'-'9']+
  | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
  | '0' ['o' 'O'] ['0'-'7']+
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_start_p in
         (* string_start_loc := Location.curr lexbuf; *)
          string lexbuf;
          lexbuf.lex_start_p <- string_start;
          STRING (get_stored_string()) }
  | "(*"
      { let comment_start = lexbuf.lex_curr_p in
        comment_depth := 1;
        begin try
          comment lexbuf
        with Lexical_error(Unterminated_comment, (Loc (_, comment_end))) ->
          raise(Lexical_error(Unterminated_comment,
                              Loc (comment_start, comment_end)))
        end;
        token lexbuf }
  | eof            {EOF}
  | _              {raise (Lexical_error (Illegal_character,
                      Loc (Lexing.lexeme_start_p lexbuf,
                      Lexing.lexeme_end_p lexbuf)))}

and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
   | "\""
      { reset_string_buffer();
        let string_start = lexbuf.lex_curr_p in
        begin try
          string lexbuf
        with Lexical_error(Unterminated_string, Loc (_, string_end)) ->
          raise(Lexical_error
            (Unterminated_string, Loc (string_start, string_end)))
        end;
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { raise(Lexical_error(Unterminated_comment, Loc(dummy_pos,
                            Lexing.lexeme_start_p lexbuf))) }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"'  'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Lexical_error(Unterminated_string, Loc (dummy_pos,
                              Lexing.lexeme_start_p lexbuf))) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

(* eof *)

