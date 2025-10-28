# Session Type Checker (STC)

An implementation of Multiparty Session Types including global types, local types, and processes.

## Architecture

The tool is written in OCaml and is broadly organised as follows:

### Core Modules
- **`ast.ml`** - Abstract syntax tree definitions for session types
- **`loc.ml`** - Location tracking for error reporting
- **`parser.mly`** - Menhir parser specification
- **`lexer.mll`** - OCamllex lexer specification

### High-Level API Modules
- **`pretty.ml`** - Pretty printing for all AST types
- **`parse.ml`** - Parsing with comprehensive error handling
- **`wellformed.ml`** - Well-formedness checking (pre-type-checking validation)
- **`check.ml`** - **Complete type checking** with variable tracking (syntax-directed typing rules)
- **`subtype.ml`** - Subtyping for local types (Gay & Hole 2005 coinductive algorithm)
- **`utils.ml`** - Shared utility functions (unfolding, De Bruijn conversion)
- **`balanced.ml`** - **Balancedness checking** for global types (ensures all branches involve same participants)
- **`interpreter.ml`** - Runtime execution with random choice and error handling

### Application
- **`main.ml`** - Command-line interface for parsing

## Building

Ensure you have OCaml, dune, menhir, and testing dependencies installed:

```bash
opam install dune menhir ocamllex alcotest
```

Build the project:

```bash
dune build
```

## VSCode Extension

A VSCode extension for syntax highlighting `.stc` files is included in the `vscode-extension/` directory.

### Quick Install

```bash
# Copy to your VSCode extensions folder
cp -r vscode-extension ~/.vscode/extensions/stc-language-0.1.0

# Restart VSCode
```

For detailed installation instructions, packaging, and publishing, see [`vscode-extension/INSTALL.md`](vscode-extension/INSTALL.md).

The extension provides:
- Syntax highlighting for all `.stc` language features
- Comment support (`//`)
- Auto-closing brackets and braces
- Bracket matching

## Execution and Interpretation

The command-line tool automatically executes valid programs with:
- **Automatic well-formedness checking** before execution
- **Random choice selection** for nondeterministic branches  
- **Step-by-step execution trace** showing all communications
- **Runtime type checking** with helpful error messages
- **Proper recursion unfolding** for recursive processes

All program files are automatically checked for:
- No duplicate process names
- All referenced processes are defined
- All processes are well-formed (no free variables, no unguarded recursion, etc.)

Example execution:
```bash
$ dune exec ./main.exe -- calculator.stc
Parsing calculator.stc...
✓ Parsing successful

Program:
────────
Calculator = rec X.client?{add: client?(a).client?(b).client![(a + b)].X, ...}
Client = server!{add: server![5].server![3].server?(result).server!{done: 0}}

main = server :: Calculator | client :: Client

Checking well-formedness...
✓ Well-formedness check passed

Executing program...
════════════════════

╔══════════════════════════════════════════╗
║  Program Execution Starting              ║
╚══════════════════════════════════════════╝

Initial Configuration:
─────────────────────
  client: server!{add: server![5].server![3].server?(result).server!{done: 0}, done: 0}
  server: rec X.client?{...}

Step 1:
────────
  client → server: label 'add'

Step 2:
────────
  client → server: 5

Step 3:
────────
  client → server: 3

Step 4:
────────
  server → client: 8

Step 5:
────────
  client → server: label 'done'

✓ All processes terminated successfully!
Total steps: 5
```

## Command-Line Interface

The main executable provides a unified CLI for working with session type programs:

```bash
# Parse, check, and execute a program (default mode)
dune exec ./main.exe -- example.stc

# Only parse and check well-formedness (no execution)
dune exec ./main.exe -- -c example.stc

# Check only mode is useful for validating programs
dune exec ./main.exe -- --check-only calculator.stc
```

### CLI Options

**Main Options:**
- `-c, --check-only` - Only parse and check well-formedness (skip execution)
- `-h, --help` - Show help message

**Other Options** (for individual type parsing):
- `-g, --global` - Parse as global type
- `-l, --local` - Parse as local type  
- `-p, --process` - Parse as process
- `-s, --string` - Parse from string instead of file

### Legacy Usage Examples

```bash
# Parse a global type from string
dune exec ./main.exe -- -g -s "p -> q:[int]; end"

# Parse a local type from string
dune exec ./main.exe -- -l -s "p?[bool]; end"

# Parse a process from string
dune exec ./main.exe -- -p -s "p![42].0"
```

### Program File Format

Program files (`.stc` extension recommended) define process definitions and a main composition:

```
ProcessName = process_body

main = participant1::ProcessName1 | participant2::ProcessName2
```

Example (`example.stc`):
```
Client = server!{request: server?(response).0, quit: 0}
Server = rec X.client?{request: client![42].X, quit: 0}

main = client::Client | server::Server
```

### Optional Type Annotations

Process definitions can include optional local type annotations for documentation and future type checking:

```
ProcessName :: local_type
ProcessName = process_body
```

Example (`dice_game.stc`):
```
Player :: dealer![Int];end
Player = dealer![1 nondet 2 nondet 3 nondet 4 nondet 5 nondet 6].0

Dealer :: player?[Int];end
Dealer = player?(roll).0

main = player::Player | dealer::Dealer
```

Type annotations are currently:
- **Parsed** and displayed in program output
- **Checked for well-formedness** (no free variables, proper recursion, etc.)
- **Not validated** against the process implementation (future feature)
- **Ignored during execution** (no runtime overhead)

This provides self-documentation and lays the groundwork for future type checking.

## Examples

Run interactive examples:

```bash
dune exec ./example.exe
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
p:label.P                     Internal choice (select label)
p?{l1:P1, l2:P2}             External choice (offer multiple labels)
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
e1 nondet e2                  Non-deterministic choice
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
9. Non-deterministic choice: `nondet`

### Comments

Comments are supported in program files:
```
// This is a comment
Client = server![42].0  // Inline comment

// Comments can span multiple lines
// Each line must start with //
```

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

(* A client that makes a request *)
let client = Parse.process_from_string
  "server:request.server?(response).0"

(* Pretty print them *)
Format.printf "%a@\n" Pretty.pp_process server;
Format.printf "%a@\n" Pretty.pp_process client
```
