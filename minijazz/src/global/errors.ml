open Location

exception Error

type lexical_error =
  | Illegal_character
  | Unterminated_comment
  | Bad_char_constant
  | Unterminated_string

let lexical_error err loc =
  Format.eprintf (match err with
    | Illegal_character -> Pervasives.format_of_string "%aIllegal character.@."
    | Unterminated_comment -> "%aUnterminated comment.@."
    | Bad_char_constant -> "%aBad char constant.@."
    | Unterminated_string -> "%aUnterminated string.@."
     ) print_location loc;
  raise Error

let syntax_error loc =
  Format.eprintf "%aSyntax error.@." print_location loc;
  raise Error
