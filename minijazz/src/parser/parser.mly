%{

open Ident
open Static
open Ast
open Location
open Misc

let fresh_param () =
  mk_static_exp (SVar ("_n"^(Misc.gen_symbol ())))

%}

%token INLINED ROM RAM WHERE END CONST PROBING
%token LPAREN RPAREN COLON COMMA EQUAL REG OR XOR NAND AND POWER SLASH
%token EOF RBRACKET LBRACKET GREATER LESS NOT SEMICOL PLUS MINUS STAR
%token IF THEN ELSE LEQ DOT DOTDOT
%token <string> NAME
%token <string> STRING
%token <int> INT
%token <string> BOOL_INT
%token <bool> BOOL

%left DOT
%left OR PLUS
%left LEQ EQUAL
%right MINUS
%left NAND XOR AND
%left STAR SLASH
%right NOT REG
%right POWER

%start program
%type <Ast.program> program

%%

/** Tools **/
%inline slist(S, x)        : l=separated_list(S, x)                    {l}
%inline snlist(S, x)       : l=separated_nonempty_list(S, x)           {l}
%inline tuple(x)           : LPAREN h=x COMMA t=snlist(COMMA,x) RPAREN { h::t }
%inline tag_option(P,x):
  |/* empty */    { None }
  | P v=x         { Some(v) }

localize(x): y=x { y, (Loc($startpos(y),$endpos(y))) }

program:
  | c=const_decs n=node_decs EOF
      { mk_program c n }

const_decs: c=list(const_dec) {c}
const_dec:
  | CONST n=name EQUAL se=static_exp option(SEMICOL)
      { mk_const_dec ~loc:(Loc($startpos,$endpos)) n se }

name: n=NAME { n }

ident:
  | n=name { ident_of_string n }

type_ident: LBRACKET se=static_exp RBRACKET { TBitArray se }

node_name:
  | n=name { reset_symbol_table (); n }

node_decs: ns=list(node_dec) { ns }
node_dec:
  inlined=inlined_status n=node_name p=params LPAREN args=args RPAREN
  EQUAL out=node_out WHERE b=block probes=probe_decls END WHERE option(SEMICOL)
      { mk_node n (Loc ($startpos,$endpos)) inlined args out p b probes }

node_out:
  | a=arg { [a] }
  | LPAREN out=args RPAREN { out }

inlined_status:
  | INLINED { Inlined }
  | /*empty*/ { NotInlined }

params:
  | /*empty*/ { [] }
  | LESS pl=snlist(COMMA,param) GREATER { pl }

param:
  n=NAME { mk_param n }

args: vl=slist(COMMA, arg) { vl }

arg:
  | n=ident COLON t=type_ident { mk_var_dec n t }
  | n=ident { mk_var_dec n TBit }

block:
  | eqs=equs { BEqs (eqs, []) }
  | IF se=static_exp THEN thenb=block ELSE elseb=block END IF { BIf(se, thenb, elseb) }

equs: eq=equ tl=equ_tail { eq::tl }
equ_tail:
  | /*empty*/ { [] }
  | SEMICOL { [] }
  | SEMICOL eq=equ tl=equ_tail { eq::tl }
equ: p=pat EQUAL e=exp { mk_equation p e }

pat:
  | n=ident                              { Evarpat n }
  | LPAREN p=snlist(COMMA, ident) RPAREN { Etuplepat p }

static_exp: se=_static_exp { mk_static_exp ~loc:(Loc ($startpos,$endpos)) se }
_static_exp :
  | i=INT { SInt i }
  | n=NAME { SVar n }
  | LPAREN se=_static_exp RPAREN { se }
  /*integer ops*/
  | se1=static_exp POWER se2=static_exp { SBinOp(SPower, se1, se2) }
  | se1=static_exp PLUS se2=static_exp { SBinOp(SAdd, se1, se2) }
  | se1=static_exp MINUS se2=static_exp { SBinOp(SMinus, se1, se2) }
  | se1=static_exp STAR se2=static_exp { SBinOp(SMult, se1, se2) }
  | se1=static_exp SLASH se2=static_exp { SBinOp(SDiv, se1, se2) }
  /*bool ops*/
  | se1=static_exp EQUAL se2=static_exp { SBinOp(SEqual, se1, se2) }
  | se1=static_exp LEQ se2=static_exp { SBinOp(SLeq, se1, se2) }

exps: LPAREN e=slist(COMMA, exp) RPAREN {e}

exp: e=_exp { mk_exp ~loc:(Loc ($startpos,$endpos)) e }
_exp:
  | e=_simple_exp  { e }
  | c=const { Econst c }
  | REG e=exp { Ereg e }
  | n=NAME p=call_params a=exps { Ecall (n, p, a) }
  | e1=exp PLUS e2=exp { Ecall ("or", [], [e1; e2]) }
  | e1=exp OR e2=exp { Ecall ("or", [], [e1; e2]) }
  | e1=exp AND e2=exp { Ecall ("and", [], [e1; e2]) }
  | e1=exp POWER e2=exp { Ecall("xor", [], [e1; e2]) }
  | e1=exp XOR e2=exp { Ecall ("xor", [], [e1; e2]) }
  | e1=exp NAND e2=exp { Ecall ("nand", [], [e1; e2]) }
  | NOT a=exp     { Ecall ("not", [], [a])}
  | e1=exp DOT e2=exp
    { Ecall("concat", [fresh_param(); fresh_param(); fresh_param ()], [e1; e2]) }
  | e1=simple_exp LBRACKET idx=static_exp RBRACKET
    { Ecall ("select", [idx; fresh_param()], [e1]) }
  | e1=simple_exp LBRACKET low=static_exp DOTDOT high=static_exp RBRACKET
    { Ecall("slice", [low; high; fresh_param()], [e1]) }
  | e1=simple_exp LBRACKET low=static_exp DOTDOT RBRACKET
    { let n = fresh_param () in
      let high = mk_static_exp (SBinOp(SMinus, n, mk_static_exp (SInt 1))) in
      Ecall("slice", [low; high; n], [e1]) }
  | e1=simple_exp LBRACKET DOTDOT high=static_exp RBRACKET
    {
      let params = [mk_static_exp (SInt 0); high; fresh_param ()] in
      Ecall("slice", params, [e1])
    }
  | ro=rom_or_ram LESS addr_size=static_exp
    COMMA word_size=static_exp input_file=tag_option(COMMA, STRING) GREATER a=exps
    { Emem(ro, addr_size, word_size, input_file, a) }

simple_exp: e=_simple_exp { mk_exp ~loc:(Loc ($startpos,$endpos)) e }
_simple_exp:
  | n=ident                   { Evar n }
  | LPAREN e=_exp RPAREN      { e }

const:
  | b=BOOL { VBit b }
  | b=BOOL_INT { VBitArray (bool_array_of_string b) }
  | i=INT
    { match i with
      | 0 -> VBit false
      | 1 -> VBit true
      | _ -> raise Parsing.Parse_error
    }
  | LBRACKET RBRACKET { VBitArray (Array.make 0 false) }

rom_or_ram :
  | ROM { MRom }
  | RAM { MRam }

call_params:
  | /*empty*/ { [] }
  | LESS pl=snlist(COMMA,static_exp) GREATER { pl }

probe_decls:
  | /*empty*/ { [] }
  | PROBING l=separated_nonempty_list(COMMA, ident) { l }
%%
