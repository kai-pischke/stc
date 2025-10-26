# Session Type Checker (STC)

A clean, modular parser for session types with global types, local types, and processes.

## Architecture

The codebase is organized into clean, focused modules:

### Core Modules
- **`ast.ml`** - Abstract syntax tree definitions for session types
- **`loc.ml`** - Location tracking for error reporting
- **`parser.mly`** - Menhir parser specification
- **`lexer.mll`** - OCamllex lexer specification

### High-Level API Modules
- **`pretty.ml`** - Pretty printing for all AST types
- **`parse.ml`** - Parsing with comprehensive error handling
- **`wellformed.ml`** - Well-formedness checking (pre-type-checking validation)
- **`interpreter.ml`** - Runtime execution with random choice and error handling

### Applications
- **`main.ml`** - Command-line interface for parsing
- **`test/`** - Test suite directory (all using Alcotest)
  - **`test_parser.ml`** - Parser tests (40 tests)
  - **`test_wellformed.ml`** - Well-formedness tests (43 tests)
  - **`test_interpreter.ml`** - Interpreter tests (11 tests)

## Building

Ensure you have OCaml, dune, menhir, and testing dependencies installed:

```bash
opam install dune menhir ocamllex alcotest
```

Build the project:

```bash
dune build
```

## Interpreter

The project includes a runtime interpreter that executes programs with:
- **Random choice selection** for nondeterministic branches
- **Type checking** at runtime with helpful error messages
- **Step-by-step execution trace** showing communication and choices

Run the interpreter examples:
```bash
dune exec ./example_interpreter.exe
```

See [INTERPRETER.md](./INTERPRETER.md) for detailed documentation.

## Testing

Run the test suite (using Alcotest):

```bash
dune test
```

This runs 94 tests total:
- **Parser tests** (40 tests) - Global types, local types, processes, and expressions
- **Well-formedness tests** (43 tests) - Structural validation checks
- **Interpreter tests** (11 tests) - Runtime execution and error handling

## Command-Line Interface

The main executable provides a CLI for parsing session types:

```bash
# Parse a global type
dune exec ./main.exe -- -g "p -> q:[int]; end"

# Parse a local type
dune exec ./main.exe -- -l "p?[bool]; end"

# Parse a process (default)
dune exec ./main.exe -- "p![42].0"

# Parse from a file
dune exec ./main.exe -- -f -g protocol.global
```

### CLI Options

- `-g, --global` - Parse as global type
- `-l, --local` - Parse as local type
- `-p, --process` - Parse as process (default)
- `-f, --file` - Read input from file
- `-h, --help` - Show help message

## Examples

Run interactive examples:

```bash
dune exec ./example.exe
```

## Usage

### Parsing

The `Parse` module provides a clean API with proper error handling:

```ocaml
open Stc

(* Parse from string *)
let global = Parse.global_from_string "p -> q:[int]; end"
let local = Parse.local_from_string "p?[bool]; end"
let process = Parse.process_from_string "p![42].0"

(* Parse from file *)
let global = Parse.global_from_file "protocol.global"

(* Error handling *)
try
  let result = Parse.global_from_string input in
  (* use result *)
with
| Parse.ParseError err ->
    Printf.eprintf "%s\n" (Parse.string_of_error err)
```

### Pretty Printing

The `Pretty` module provides formatters for all AST types:

```ocaml
open Stc

let global = Parse.global_from_string "rec X.end"

(* Print to stdout *)
Format.printf "%a@\n" Pretty.pp_global global

(* Convert to string *)
let str = Pretty.string_of_global global
```

## Syntax Reference

### Global Types
```
end                           End of protocol
X                             Type variable
rec X.G                       Recursive type
p -> q:[B]; G                 Message from p to q with base type B
p -> q{l1:G1, l2:G2}         Choice from p to q with labels
G1 | G2                       Parallel composition
```

### Local Types
```
end                           End of protocol
X                             Type variable
rec X.T                       Recursive type
p?[B]; T                      Receive base type B from p
p![B]; T                      Send base type B to p
p?{l1:T1, l2:T2}             External choice (receive label)
p!{l1:T1, l2:T2}             Internal choice (send label)
```

### Processes
```
0                             Inactive process
X                             Process variable
rec X.P                       Recursive process
p![e].P                       Send expression e to p
p?(x).P                       Receive from p and bind to x
p!{l1:P1, l2:P2}             Internal choice (select label)
p?{l1:P1, l2:P2}             External choice (offer labels)
```

### Expressions
```
true, false                   Boolean literals
42                            Integer literals
x                             Variables
not e                         Logical negation
e1 or e2                      Logical disjunction
e1 and e2                     Logical conjunction
e1 + e2                       Addition
e1 - e2                       Subtraction
e1 * e2                       Multiplication
e1 / e2                       Integer division
e1 mod e2                     Modulo
e1 < e2                       Less than
e1 > e2                       Greater than
e1 <= e2                      Less than or equal
e1 >= e2                      Greater than or equal
e1 = e2                       Equality
(e)                           Parenthesized expression
```

#### Operator Precedence (highest to lowest)
1. Parentheses `()`
2. Logical NOT `not`
3. Multiplicative: `*`, `/`, `mod`
4. Additive: `+`, `-`
5. Comparison: `<`, `>`, `<=`, `>=`
6. Equality: `=`
7. Logical AND: `and`
8. Logical OR: `or`

## Examples

### Simple Protocol

```ocaml
(* Global: p sends an integer to q, then ends *)
let g = Parse.global_from_string "p -> q:[int]; end"

(* Local view at p: send int, then end *)
let l_p = Parse.local_from_string "q![int]; end"

(* Local view at q: receive int, then end *)
let l_q = Parse.local_from_string "p?[int]; end"

(* Process at p: send value 42 to q *)
let p = Parse.process_from_string "q![42].0"

(* Process at q: receive value and bind to x *)
let q = Parse.process_from_string "p?(x).0"
```

### Recursive Protocol

```ocaml
(* A server that repeatedly accepts requests *)
let server = Parse.process_from_string 
  "rec X.client?{request: client![true].X, quit: 0}"

(* Pretty print it *)
Format.printf "%a@\n" Pretty.pp_process server
```

## Module Interface

### Parse Module

```ocaml
type error = {
  message : string;
  line : int;
  column : int;
  position : int;
}

exception ParseError of error

val string_of_error : error -> string
val global_from_string : string -> string Ast.global
val local_from_string : string -> string Ast.local
val process_from_string : string -> string Ast.processes
val global_from_file : string -> string Ast.global
val local_from_file : string -> string Ast.local
val process_from_file : string -> string Ast.processes
```

### Pretty Module

```ocaml
val pp_global : Format.formatter -> string Ast.global -> unit
val pp_local : Format.formatter -> string Ast.local -> unit
val pp_process : Format.formatter -> string Ast.processes -> unit
val pp_expr : Format.formatter -> string Ast.expr -> unit
val string_of_global : string Ast.global -> string
val string_of_local : string Ast.local -> string
val string_of_process : string Ast.processes -> string
val string_of_expr : string Ast.expr -> string
```

## Development

### Clean build artifacts:
```bash
dune clean
```

### Run tests with verbose output:
```bash
dune test --verbose
```

### Run specific test suite:
```bash
# Run all parser tests only
dune exec ./test/test_parser.exe

# Run all well-formedness tests only
dune exec ./test/test_wellformed.exe

# Run all interpreter tests only
dune exec ./test/test_interpreter.exe

# Run a specific test within a suite
dune exec ./test/test_parser.exe -- test "processes" "send integer"
```

### Build documentation:
```bash
dune build @doc
```

### Continuous testing:
```bash
dune test --watch
```

## Additional Documentation

- **[WELLFORMEDNESS.md](./WELLFORMEDNESS.md)** - Detailed documentation of well-formedness checks
- **[PROGRAMS.md](./PROGRAMS.md)** - Documentation for program file parsing
- **[INTERPRETER.md](./INTERPRETER.md)** - Documentation for the runtime interpreter

## Next Steps

This parser provides a solid foundation for:
- Type checking session protocols
- Projection from global to local types
- Session type equivalence checking
- Deadlock detection
- Protocol verification

See `example.ml` for more usage patterns.
