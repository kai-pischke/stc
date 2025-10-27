(** Example: Using the type checker module programmatically *)

open Stc

(** Example 1: Successful type check *)
let example_correct () =
  Printf.printf "=== Example 1: Correct Process ===\n";
  
  let proc = Parse.process_from_string "server![42].server?(x).0" in
  let typ = Parse.local_from_string "server![Int];server?[Int];end" in
  
  Printf.printf "Process: %s\n" (Pretty.string_of_process proc);
  Printf.printf "Type:    %s\n" (Pretty.string_of_local typ);
  
  try
    Check.check_process proc typ;
    Printf.printf "✓ Type check passed!\n\n"
  with
  | Check.TypeError err ->
      Printf.printf "✗ Type error: %s\n\n" (Check.string_of_error err)

(** Example 2: Type mismatch *)
let example_type_mismatch () =
  Printf.printf "=== Example 2: Type Mismatch ===\n";
  
  let proc = Parse.process_from_string "server![true].0" in
  let typ = Parse.local_from_string "server![Int];end" in
  
  Printf.printf "Process: %s\n" (Pretty.string_of_process proc);
  Printf.printf "Type:    %s\n" (Pretty.string_of_local typ);
  
  try
    Check.check_process proc typ;
    Printf.printf "✓ Type check passed!\n\n"
  with
  | Check.TypeError err ->
      Printf.printf "✗ Type error: %s\n\n" (Check.string_of_error err)

(** Example 3: Role mismatch *)
let example_role_mismatch () =
  Printf.printf "=== Example 3: Role Mismatch ===\n";
  
  let proc = Parse.process_from_string "alice![42].0" in
  let typ = Parse.local_from_string "bob![Int];end" in
  
  Printf.printf "Process: %s\n" (Pretty.string_of_process proc);
  Printf.printf "Type:    %s\n" (Pretty.string_of_local typ);
  
  try
    Check.check_process proc typ;
    Printf.printf "✓ Type check passed!\n\n"
  with
  | Check.TypeError err ->
      Printf.printf "✗ Type error: %s\n\n" (Check.string_of_error err)

(** Example 4: Complex protocol with choices *)
let example_complex () =
  Printf.printf "=== Example 4: Complex Protocol ===\n";
  
  let proc = Parse.process_from_string 
    "rec X.client?{add: client?(a).client?(b).client![(a + b)].X, done: 0}" in
  let typ = Parse.local_from_string
    "rec Y.client?{add: client?[Int];client?[Int];client![Int];Y, done: end}" in
  
  Printf.printf "Process: Calculator service\n";
  Printf.printf "Type:    Recursive addition service\n";
  
  try
    Check.check_process proc typ;
    Printf.printf "✓ Type check passed!\n\n"
  with
  | Check.TypeError err ->
      Printf.printf "✗ Type error: %s\n\n" (Check.string_of_error err)

(** Example 5: Missing branch in external choice *)
let example_missing_branch () =
  Printf.printf "=== Example 5: Missing Branch ===\n";
  
  let proc = Parse.process_from_string "s?{ok: 0}" in
  let typ = Parse.local_from_string "s?{ok: end, error: end}" in
  
  Printf.printf "Process: %s\n" (Pretty.string_of_process proc);
  Printf.printf "Type:    %s\n" (Pretty.string_of_local typ);
  
  try
    Check.check_process proc typ;
    Printf.printf "✓ Type check passed!\n\n"
  with
  | Check.TypeError err ->
      Printf.printf "✗ Type error: %s\n\n" (Check.string_of_error err)

(** Run all examples *)
let () =
  Printf.printf "\n╔══════════════════════════════════════════╗\n";
  Printf.printf "║    Type Checker Examples                 ║\n";
  Printf.printf "╚══════════════════════════════════════════╝\n\n";
  
  example_correct ();
  example_type_mismatch ();
  example_role_mismatch ();
  example_complex ();
  example_missing_branch ();
  
  Printf.printf "All examples completed!\n"


