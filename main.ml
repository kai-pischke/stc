(* main.ml: Command-line interface for session type checker *)

open Stc

(** Print usage information *)
let usage () =
  Printf.printf "Session Type Checker - Command Line Interface\n\n";
  Printf.printf "Usage:\n";
  Printf.printf "  %s [options] <program-file>\n\n" Sys.argv.(0);
  Printf.printf "Main Options:\n";
  Printf.printf "  -c, --check-only   Only parse and check well-formedness (no execution)\n";
  Printf.printf "  -h, --help         Show this help message\n\n";
  Printf.printf "Visualization Options:\n";
  Printf.printf "  --graph-dot FILE   Generate DOT file from global type\n";
  Printf.printf "  --graph-png FILE   Generate PNG image from global type (requires graphviz)\n\n";
  Printf.printf "Type Parsing:\n";
  Printf.printf "  -g, --global       Parse as global type (string or file)\n";
  Printf.printf "  -l, --local        Parse as local type (string or file)\n";
  Printf.printf "  -p, --process      Parse as process (string or file)\n";
  Printf.printf "  -s, --string       Parse from string instead of file\n\n";
  Printf.printf "Examples:\n";
  Printf.printf "  %s program.stc              # Parse, check, and execute\n" Sys.argv.(0);
  Printf.printf "  %s -c program.stc           # Only parse and check\n" Sys.argv.(0);
  Printf.printf "  %s -g -s \"p -> q:[int]; end\"  # Parse global type from string\n" Sys.argv.(0);
  Printf.printf "  %s -g --graph-png out.png protocol.stc  # Visualize global type\n" Sys.argv.(0);
  exit 0

(** Parse mode for legacy commands *)
type mode = Program | Global | Local | Process

(** Process a program file: parse, check well-formedness, and optionally execute *)
let process_program filename ~check_only =
  try
    (* Parse the program *)
    Printf.printf "Parsing %s...\n" filename;
    let prog = Parse.program_from_file filename in
    Printf.printf "✓ Parsing successful\n\n";
    
    (* Display the parsed program *)
    Printf.printf "Program:\n";
    Printf.printf "────────\n";
    Format.printf "%a@\n@\n" Pretty.pp_program prog;
    
    (* Check well-formedness *)
    Printf.printf "Checking well-formedness...\n";
    Wellformed.check_program prog;
    Printf.printf "✓ Well-formedness check passed\n\n";
    
    (* Type check annotated processes *)
    Printf.printf "Type checking annotated processes...\n";
    let type_checked = ref 0 in
    List.iter (fun def ->
      match def.Ast.type_annotation with
      | Some typ ->
          Printf.printf "  Checking %s :: %s\n" 
            def.Ast.name 
            (Pretty.string_of_local typ);
          Check.check_process def.Ast.body typ;
          incr type_checked
      | None -> ()
    ) prog.Ast.definitions;
    if !type_checked = 0 then
      Printf.printf "  (no type annotations found)\n"
    else
      Printf.printf "✓ All %d annotated process(es) passed type checking\n" !type_checked;
    Printf.printf "\n";
    
    (* Execute if requested *)
    if not check_only then (
      Printf.printf "Executing program...\n";
      Printf.printf "════════════════════\n\n";
      Interpreter.execute_program prog
    ) else (
      Printf.printf "Skipping execution (check-only mode)\n"
    )
  with
  | Parse.ParseError err ->
      Printf.eprintf "\n✗ Parse Error: %s\n" (Parse.string_of_error err);
      exit 1
  | Wellformed.IllFormed err ->
      Printf.eprintf "\n✗ Well-formedness Error: %s\n" (Wellformed.string_of_error err);
      exit 1
  | Check.TypeError err ->
      Printf.eprintf "\n✗ Type Error: %s\n" (Check.string_of_error err);
      exit 1
  | Interpreter.RuntimeError err ->
      Printf.eprintf "\n✗ Runtime Error: %s\n" (Interpreter.string_of_error err);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "\n✗ File Error: %s\n" msg;
      exit 1

(** Generate DOT file from global type *)
let generate_dot_file global_type output_file =
  try
    let (graph, entry) = Type_graph.global_to_graph global_type in
    let dot_content = Type_graph.global_to_dot graph entry in
    let oc = open_out output_file in
    output_string oc dot_content;
    close_out oc;
    Printf.printf "✓ DOT file saved to: %s\n" output_file
  with
  | Sys_error msg ->
      Printf.eprintf "✗ Error writing DOT file: %s\n" msg;
      exit 1

(** Generate PNG from DOT file using graphviz *)
let dot_to_png dot_file png_file =
  let cmd = Printf.sprintf "dot -Tpng %s -o %s" 
    (Filename.quote dot_file) (Filename.quote png_file) in
  Printf.printf "Running: %s\n" cmd;
  match Sys.command cmd with
  | 0 -> 
      Printf.printf "✓ PNG image saved to: %s\n" png_file;
      true
  | _ -> 
      Printf.eprintf "✗ Error: Failed to generate PNG. Is graphviz installed?\n";
      Printf.eprintf "  Install with: brew install graphviz (macOS) or apt install graphviz (Linux)\n";
      false

(** Generate PNG file from global type *)
let generate_png_file global_type output_file =
  (* Create temporary DOT file *)
  let temp_dot = Filename.temp_file "session_type" ".dot" in
  try
    generate_dot_file global_type temp_dot;
    let success = dot_to_png temp_dot output_file in
    (* Clean up temp file *)
    (try Sys.remove temp_dot with _ -> ());
    if not success then exit 1
  with e ->
    (try Sys.remove temp_dot with _ -> ());
    raise e

(** Parse global type and display *)
let parse_global ~from_string ~dot_output ~png_output input =
  try
    let result = 
      if from_string then Parse.global_from_string input
      else Parse.global_from_file input
    in
    let source = if from_string then "String" else "File: " ^ input in
    Format.printf "@[<v>Source: %s@,@,Parsed:@,  %a@,@,Type: Global@]@." source Pretty.pp_global result;
    
    (* Check well-formedness *)
    Printf.printf "\nChecking well-formedness...\n";
    Wellformed.check_global result;
    Printf.printf "✓ Well-formedness check passed\n";
    
    (* Generate visualizations if requested *)
    (match dot_output with
     | Some file -> 
         Printf.printf "\nGenerating DOT visualization...\n";
         generate_dot_file result file
     | None -> ());
    
    (match png_output with
     | Some file ->
         Printf.printf "\nGenerating PNG visualization...\n";
         generate_png_file result file
     | None -> ())
  with
  | Parse.ParseError err ->
      Printf.eprintf "\n✗ Parse Error: %s\n" (Parse.string_of_error err);
      exit 1
  | Wellformed.IllFormed err ->
      Printf.eprintf "\n✗ Well-formedness Error: %s\n" (Wellformed.string_of_error err);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "\n✗ File Error: %s\n" msg;
      exit 1

(** Parse local type and display *)
let parse_local ~from_string input =
  try
    let result = 
      if from_string then Parse.local_from_string input
      else Parse.local_from_file input
    in
    let source = if from_string then "String" else "File: " ^ input in
    Format.printf "@[<v>Source: %s@,@,Parsed:@,  %a@,@,Type: Local@]@." source Pretty.pp_local result;
    
    (* Check well-formedness *)
    Printf.printf "\nChecking well-formedness...\n";
    Wellformed.check_local result;
    Printf.printf "✓ Well-formedness check passed\n"
  with
  | Parse.ParseError err ->
      Printf.eprintf "\n✗ Parse Error: %s\n" (Parse.string_of_error err);
      exit 1
  | Wellformed.IllFormed err ->
      Printf.eprintf "\n✗ Well-formedness Error: %s\n" (Wellformed.string_of_error err);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "\n✗ File Error: %s\n" msg;
      exit 1

(** Parse process and display *)
let parse_process ~from_string input =
  try
    let result = 
      if from_string then Parse.process_from_string input
      else Parse.process_from_file input
    in
    let source = if from_string then "String" else "File: " ^ input in
    Format.printf "@[<v>Source: %s@,@,Parsed:@,  %a@,@,Type: Process@]@." source Pretty.pp_process result;
    
    (* Check well-formedness *)
    Printf.printf "\nChecking well-formedness...\n";
    Wellformed.check_process result;
    Printf.printf "✓ Well-formedness check passed\n"
  with
  | Parse.ParseError err ->
      Printf.eprintf "\n✗ Parse Error: %s\n" (Parse.string_of_error err);
      exit 1
  | Wellformed.IllFormed err ->
      Printf.eprintf "\n✗ Well-formedness Error: %s\n" (Wellformed.string_of_error err);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "\n✗ File Error: %s\n" msg;
      exit 1

(** Main entry point *)
let () =
  let mode = ref Program in
  let from_string = ref false in
  let check_only = ref false in
  let dot_output = ref None in
  let png_output = ref None in
  let input = ref None in

  let set_mode m () = mode := m in
  let set_string () = from_string := true in
  let set_check_only () = check_only := true in
  let set_dot_output s = dot_output := Some s in
  let set_png_output s = png_output := Some s in
  let set_input s = input := Some s in

  let spec = [
    (* Main options *)
    "-c", Arg.Unit set_check_only, "Only check well-formedness (no execution)";
    "--check-only", Arg.Unit set_check_only, "Only check well-formedness (no execution)";
    "-h", Arg.Unit usage, "Show help";
    "--help", Arg.Unit usage, "Show help";
    
    (* Visualization options *)
    "--graph-dot", Arg.String set_dot_output, "Generate DOT file from global type";
    "--graph-png", Arg.String set_png_output, "Generate PNG image from global type";
    
    (* Legacy options for individual types *)
    "-g", Arg.Unit (set_mode Global), "Parse as global type";
    "--global", Arg.Unit (set_mode Global), "Parse as global type";
    "-l", Arg.Unit (set_mode Local), "Parse as local type";
    "--local", Arg.Unit (set_mode Local), "Parse as local type";
    "-p", Arg.Unit (set_mode Process), "Parse as process";
    "--process", Arg.Unit (set_mode Process), "Parse as process";
    "-s", Arg.Unit set_string, "Parse from string";
    "--string", Arg.Unit set_string, "Parse from string";
  ] in

  Arg.parse spec set_input "Session Type Checker";

  match !input with
  | None ->
      Printf.eprintf "Error: No input provided\n\n";
      usage ()
  | Some inp ->
      (* Check if visualization options are used with non-global types *)
      if (!dot_output <> None || !png_output <> None) && !mode <> Global then (
        Printf.eprintf "Error: Visualization options (--graph-dot, --graph-png) only work with global types\n";
        Printf.eprintf "Use -g or --global to parse as a global type\n";
        exit 1
      );
      
      match !mode with
      | Program -> 
          if !from_string then (
            Printf.eprintf "Error: String input not supported for programs\n";
            Printf.eprintf "Programs must be read from files\n";
            exit 1
          );
          process_program inp ~check_only:!check_only
      | Global -> 
          parse_global ~from_string:!from_string 
                       ~dot_output:!dot_output 
                       ~png_output:!png_output 
                       inp
      | Local -> parse_local ~from_string:!from_string inp
      | Process -> parse_process ~from_string:!from_string inp
