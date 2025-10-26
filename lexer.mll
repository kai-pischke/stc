{
open Parser
exception Error of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_')*

rule token = parse
  | white      { token lexbuf }
  | newline    { Lexing.new_line lexbuf; token lexbuf }
  | "end"      { END }
  | "rec"      { REC }
  | "main"     { MAIN }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "true"     { ETRUE }
  | "false"    { EFALSE }
  | "not"      { NOT }
  | "or"       { OR }
  | "and"      { AND }
  | "mod"      { MOD }
  | "->"       { ARROW }
  | "::"       { COLONCOLON }
  | "?"        { QUEST }
  | "!"        { BANG }
  | "["        { LBRACK }
  | "]"        { RBRACK }
  | "{"        { LBRACE }
  | "}"        { RBRACE }
  | ":"        { COLON }
  | ","        { COMMA }
  | "|"        { BAR }
  | "."        { DOT }
  | ";"        { SEMI }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "*"        { TIMES }
  | "/"        { DIV }
  | "<="       { LE }
  | "<"        { LT }
  | ">="       { GE }
  | ">"        { GT }
  | "="        { EQ }
  | digit+ as n { EINT (int_of_string n) }
  | ident as id { IDENT id }
  | eof        { EOF }
  | _ as c     { raise (Error (Printf.sprintf "Unexpected character: %c" c)) }

