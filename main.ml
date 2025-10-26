(* main.ml: Command-line interface for session type parser *)

open Stc

(** Print usage information *)
let usage () =
  Printf.printf "Session Type Parser - Command Line Interface\n\n";
  Printf.printf "Usage:\n";
  Printf.printf "  %s [options] <input>\n\n" Sys.argv.(0);
  Printf.printf "Options:\n";
  Printf.printf "  -g, --global      Parse as global type\n";
  Printf.printf "  -l, --local       Parse as local type\n";
  Printf.printf "  -p, --process     Parse as process (default)\n";
  Printf.printf "  -f, --file        Read input from file instead of command line\n";
  Printf.printf "  -h, --help        Show this help message\n\n";
  Printf.printf "Examples:\n";
  Printf.printf "  %s -g \"p -> q:[int]; end\"\n" Sys.argv.(0);
  Printf.printf "  %s -l \"p?[bool]; end\"\n" Sys.argv.(0);
  Printf.printf "  %s -p \"p![42].0\"\n" Sys.argv.(0);
  Printf.printf "  %s -f -g protocol.global\n" Sys.argv.(0);
  exit 0

(** Parse mode *)
type mode = Global | Local | Process

(** Parse global type and display *)
let parse_global_string input =
  try
    let result = Parse.global_from_string input in
    Format.printf "@[<v>Input:@,  %s@,@,Parsed:@,  %a@,@,Type: Global@]@."
      input Pretty.pp_global result
  with
  | Parse.ParseError err ->
      Printf.eprintf "Error: %s\n" (Parse.string_of_error err);
      exit 1

(** Parse local type and display *)
let parse_local_string input =
  try
    let result = Parse.local_from_string input in
    Format.printf "@[<v>Input:@,  %s@,@,Parsed:@,  %a@,@,Type: Local@]@."
      input Pretty.pp_local result
  with
  | Parse.ParseError err ->
      Printf.eprintf "Error: %s\n" (Parse.string_of_error err);
      exit 1

(** Parse process and display *)
let parse_process_string input =
  try
    let result = Parse.process_from_string input in
    Format.printf "@[<v>Input:@,  %s@,@,Parsed:@,  %a@,@,Type: Process@]@."
      input Pretty.pp_process result
  with
  | Parse.ParseError err ->
      Printf.eprintf "Error: %s\n" (Parse.string_of_error err);
      exit 1

(** Parse global type from file and display *)
let parse_global_file filename =
  try
    let result = Parse.global_from_file filename in
    Format.printf "@[<v>File: %s@,@,Parsed:@,  %a@,@,Type: Global@]@."
      filename Pretty.pp_global result
  with
  | Parse.ParseError err ->
      Printf.eprintf "Error: %s\n" (Parse.string_of_error err);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "File error: %s\n" msg;
      exit 1

(** Parse local type from file and display *)
let parse_local_file filename =
  try
    let result = Parse.local_from_file filename in
    Format.printf "@[<v>File: %s@,@,Parsed:@,  %a@,@,Type: Local@]@."
      filename Pretty.pp_local result
  with
  | Parse.ParseError err ->
      Printf.eprintf "Error: %s\n" (Parse.string_of_error err);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "File error: %s\n" msg;
      exit 1

(** Parse process from file and display *)
let parse_process_file filename =
  try
    let result = Parse.process_from_file filename in
    Format.printf "@[<v>File: %s@,@,Parsed:@,  %a@,@,Type: Process@]@."
      filename Pretty.pp_process result
  with
  | Parse.ParseError err ->
      Printf.eprintf "Error: %s\n" (Parse.string_of_error err);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "File error: %s\n" msg;
      exit 1

(** Main entry point *)
let () =
  let mode = ref Process in
  let from_file = ref false in
  let input = ref None in

  let set_mode m () = mode := m in
  let set_file () = from_file := true in
  let set_input s = input := Some s in

  let spec = [
    "-g", Arg.Unit (set_mode Global), "Parse as global type";
    "--global", Arg.Unit (set_mode Global), "Parse as global type";
    "-l", Arg.Unit (set_mode Local), "Parse as local type";
    "--local", Arg.Unit (set_mode Local), "Parse as local type";
    "-p", Arg.Unit (set_mode Process), "Parse as process";
    "--process", Arg.Unit (set_mode Process), "Parse as process";
    "-f", Arg.Unit set_file, "Read from file";
    "--file", Arg.Unit set_file, "Read from file";
    "-h", Arg.Unit usage, "Show help";
    "--help", Arg.Unit usage, "Show help";
  ] in

  Arg.parse spec set_input "Session Type Parser";

  match !input with
  | None ->
      Printf.eprintf "Error: No input provided\n\n";
      usage ()
  | Some inp ->
      if !from_file then
        match !mode with
        | Global -> parse_global_file inp
        | Local -> parse_local_file inp
        | Process -> parse_process_file inp
      else
        match !mode with
        | Global -> parse_global_string inp
        | Local -> parse_local_string inp
        | Process -> parse_process_string inp
