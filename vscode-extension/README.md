# Session Type Checker Language Support

Syntax highlighting for Session Type Checker (`.stc`) files in Visual Studio Code.

## Features

- **Syntax Highlighting**: Full syntax highlighting for `.stc` files including:
  - Keywords: `if`, `then`, `else`, `rec`, `main`, `end`
  - Logical operators: `and`, `or`, `not`, `nondet`, `mod`
  - Boolean literals: `true`, `false`
  - Session type operators: `!`, `?`, `:`, `->`, `::`
  - Comments: `// comment`
  - Identifiers and process names

- **Comment Support**: Line comments with `//`

- **Auto-closing pairs**: Automatic closing of brackets, parentheses, and braces

- **Bracket matching**: Matching brackets, braces, and parentheses

## Syntax Examples

### Process Definitions
```stc
Client = server:quit.0
Server = rec X.client?{request: client![42].X, quit: 0}

main = client::Client | server::Server
```

### Expressions with Non-deterministic Choice
```stc
Sender = receiver![10 nondet 20].0
Receiver = sender?(x).0
```

### Comments
```stc
// This is a comment
Process = role![value].0  // Inline comment
```

## Installation

### From Source

1. Copy the `vscode-extension` directory to your VSCode extensions folder:
   - **Windows**: `%USERPROFILE%\.vscode\extensions\`
   - **macOS/Linux**: `~/.vscode/extensions/`

2. Restart VSCode

### Using VSCE (Package and Install)

1. Install `vsce` (Visual Studio Code Extension Manager):
   ```bash
   npm install -g @vscode/vsce
   ```

2. Package the extension:
   ```bash
   cd vscode-extension
   vsce package
   ```

3. Install the `.vsix` file:
   - Open VSCode
   - Go to Extensions (Ctrl+Shift+X / Cmd+Shift+X)
   - Click the "..." menu → "Install from VSIX..."
   - Select the generated `.vsix` file

## Language Features

### Keywords
- Control flow: `if`, `then`, `else`
- Recursion: `rec`
- Program entry: `main`
- Termination: `end`

### Operators
- Send: `!`
- Receive: `?`
- Internal choice: `:`
- Parallel composition: `|`
- Arrow: `->`
- Label binding: `::`

### Logical Operators
- `and`, `or`, `not`
- `nondet` - non-deterministic choice

### Literals
- Booleans: `true`, `false`
- Integers: `0`, `42`, etc.

## Session Type Syntax

- **Send**: `role![expression].continuation`
- **Receive**: `role?(variable).continuation`
- **Internal Choice**: `role:label.continuation`
- **External Choice**: `role?{label1: P1, label2: P2}`
- **Recursion**: `rec X.process`
- **Parallel**: `P1 | P2`

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

MIT License

## Links

- [Session Type Checker Repository](https://github.com/yourusername/stc)
- [Session Types Overview](https://en.wikipedia.org/wiki/Session_type)

