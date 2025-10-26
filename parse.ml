(* parse.ml: Parsing interface with error handling *)

(** Parse error with location information *)
type error = {
  message : string;
  line : int;
  column : int;
  position : int;
}

exception ParseError of error

(** Create error from lexbuf and message *)
let make_error lexbuf message =
  let pos = Lexing.lexeme_start_p lexbuf in
  {
    message;
    line = pos.pos_lnum;
    column = pos.pos_cnum - pos.pos_bol;
    position = pos.pos_cnum;
  }

(** Format error for display *)
let string_of_error err =
  Format.sprintf "Parse error at line %d, column %d: %s"
    err.line err.column err.message

(** Parse global type from string *)
let global_from_string input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.gfile Lexer.token lexbuf
  with
  | Lexer.Error msg ->
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

(** Parse local type from string *)
let local_from_string input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.lfile Lexer.token lexbuf
  with
  | Lexer.Error msg ->
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

(** Parse process from string *)
let process_from_string input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.pfile Lexer.token lexbuf
  with
  | Lexer.Error msg ->
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

(** Parse global type from file *)
let global_from_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let result = Parser.gfile Lexer.token lexbuf in
    close_in ic;
    result
  with
  | Lexer.Error msg ->
      close_in ic;
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      close_in ic;
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

(** Parse local type from file *)
let local_from_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let result = Parser.lfile Lexer.token lexbuf in
    close_in ic;
    result
  with
  | Lexer.Error msg ->
      close_in ic;
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      close_in ic;
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

(** Parse process from file *)
let process_from_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let result = Parser.pfile Lexer.token lexbuf in
    close_in ic;
    result
  with
  | Lexer.Error msg ->
      close_in ic;
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      close_in ic;
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

(** Parse program from string *)
let program_from_string input =
  let lexbuf = Lexing.from_string input in
  try
    Parser.program_file Lexer.token lexbuf
  with
  | Lexer.Error msg ->
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

(** Parse program from file *)
let program_from_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let result = Parser.program_file Lexer.token lexbuf in
    close_in ic;
    result
  with
  | Lexer.Error msg ->
      close_in ic;
      let err = make_error lexbuf msg in
      raise (ParseError err)
  | Parser.Error ->
      close_in ic;
      let err = make_error lexbuf "syntax error" in
      raise (ParseError err)

