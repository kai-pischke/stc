# Changelog

All notable changes to the Session Type Checker Language Support extension will be documented in this file.

## [0.1.0] - 2025-10-27

### Added
- Initial release of Session Type Checker language support
- Syntax highlighting for `.stc` files
- Support for keywords: `if`, `then`, `else`, `rec`, `main`, `end`
- Support for operators: `!`, `?`, `:`, `->`, `::`
- Support for logical operators: `and`, `or`, `not`, `nondet`, `mod`
- Line comment support with `//`
- Auto-closing pairs for brackets, braces, and parentheses
- Bracket matching
- Syntax highlighting for:
  - Process definitions
  - Session type operators
  - Expressions with non-deterministic choice
  - Integer and boolean literals
  - Identifiers and process names
  - Comments

### Features
- Full TextMate grammar for `.stc` files
- Language configuration for VSCode
- Support for process syntax: `p![e].P`, `p?(x).P`, `p:label.P`
- Support for external choice: `p?{l1:P1, l2:P2}`
- Support for parallel composition: `P1 | P2`
- Support for recursion: `rec X.P`
- Expression syntax highlighting including arithmetic, boolean, and comparison operators
- Non-deterministic choice operator: `nondet`

## Future Enhancements

Planned features for future releases:
- Code snippets for common patterns
- Hover information for keywords and operators
- Go to definition for process names
- Code folding
- Semantic highlighting
- Diagnostics and error checking
- Integration with the STC compiler
- Auto-completion for process names
- Rename refactoring


