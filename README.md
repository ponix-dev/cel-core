# CEL-Core

A complete [Common Expression Language](https://github.com/google/cel-spec) (CEL) implementation in Rust, validated through the official cel-spec conformance test suite.

CEL-Core provides the full CEL processing pipeline—parsing, type checking, and evaluation—with the goal of achieving implementation parity with [cel-go](https://github.com/google/cel-go), the reference implementation.

> **Note:** This project is a work in progress. See the [Roadmap](ROADMAP.md) for implementation status and the [conformance tests](crates/cel-core-conformance) for cel-spec compatibility.

Built on top of the core implementation is a Language Server Protocol (LSP) server that brings CEL support to any LSP-compatible editor.

## Features

### CEL Implementation
- **Parser** - Full CEL syntax support with error recovery
- **Type Checker** - Type inference and validation
- **Evaluator** - Expression evaluation with variable bindings
- **Conformance Testing** - Validated against the official cel-spec test suite

### Language Server
- **Syntax Highlighting** - Semantic tokens for accurate code coloring
- **Error Diagnostics** - Real-time parsing and validation errors
- **Hover Information** - Type and function documentation on hover
- **Protovalidate Support** - CEL validation in `.proto` files

## Crate Structure

This project is organized as a Cargo workspace with the following crates:

| Crate | Description |
|-------|-------------|
| **cel-core-parser** | CEL expression parser with error recovery. Uses `logos` for lexing and a hand-written recursive descent parser. Produces an AST with span tracking for diagnostics. |
| **cel-core-proto** | Protobuf types and bidirectional conversion between the parser AST and cel-spec proto format. Enables wire compatibility with other CEL implementations. |
| **cel-core-lsp** | Language Server Protocol implementation using `tower-lsp`. Provides diagnostics, hover, and semantic tokens for IDE integration. |
| **cel-core-conformance** | Conformance testing against the official [cel-spec](https://github.com/google/cel-spec) test suite. Not published to crates.io. |

```
crates/
  cel-core-parser/       # Lexer + parser, produces AST
  cel-core-proto/        # Bidirectional AST <-> proto conversion
  cel-core-lsp/          # Language Server Protocol implementation
  cel-core-conformance/  # Conformance testing against cel-spec
```

## Roadmap

CEL-Core is working toward full CEL implementation parity with cel-go. See [ROADMAP.md](ROADMAP.md) for the detailed implementation plan.

### Current Status

| Component | Status | Notes |
|-----------|--------|-------|
| **Lexer** | Complete | Logos-based, all CEL tokens |
| **Parser** | Complete | Hand-written recursive descent, error recovery |
| **AST** | Complete | All expression types, span tracking |
| **Proto Conversion** | Partial | AST to/from `ParsedExpr`, no `CheckedExpr` |
| **LSP** | Partial | Diagnostics, hover, semantic tokens |
| **Type Checking** | Not started | No `CheckedExpr` production |
| **Evaluation** | Not started | No runtime execution |

### Milestones

1. **Basic Evaluation** - Value types, literals, operators, variable binding
2. **Collections and Strings** - List/map support, string functions
3. **Type Checking** - Full type inference and `CheckedExpr` production
4. **Macros and Comprehensions** - `all`, `exists`, `map`, `filter`
5. **Full Conformance** - Pass all cel-spec conformance tests

## Development

### First-time Setup

This project uses [mise](https://mise.jdx.dev/) for tool management and task running.

```bash
# Trust and install mise tools
mise trust
mise install

# Initialize git submodules (required for conformance tests)
mise run conformance:setup
```

### Testing

```bash
# Run all tests
mise run test

# Run unit tests only
mise run test:unit

# Run integration tests
mise run test:integration

# Run conformance tests (requires submodule setup)
mise run conformance:test
```

### Protobuf Generation

```bash
# Regenerate Rust code from cel-spec protobuf definitions
mise run proto:generate
```

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## References

- [CEL Spec](https://github.com/google/cel-spec) - Official specification and test suite
- [cel-go](https://github.com/google/cel-go) - Reference implementation
- [CEL Language Definition](https://github.com/google/cel-spec/blob/master/doc/langdef.md)
