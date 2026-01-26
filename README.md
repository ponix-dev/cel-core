# CEL-Core

A complete [Common Expression Language](https://github.com/google/cel-spec) (CEL) implementation in Rust, validated through the official cel-spec conformance test suite.

CEL-Core provides the full CEL processing pipeline—parsing, type checking, and proto wire format conversion—with the goal of achieving implementation parity with [cel-go](https://github.com/google/cel-go), the reference implementation.

> **Note:** This project is a work in progress. See the [Roadmap](ROADMAP.md) for implementation status and the [conformance tests](crates/cel-core-conformance) for cel-spec compatibility.

Built on top of the core implementation is a Language Server Protocol (LSP) server that brings CEL support to any LSP-compatible editor.

## Features

### CEL Implementation
- **Parser** - Full CEL syntax support with error recovery
- **Type Checker** - Type inference, overload resolution, and validation
- **Standard Library** - All standard CEL functions and operators
- **Extensions** - String, math, encoders, and optionals extensions
- **Proto Types** - Support for protobuf message types in expressions
- **Conformance Testing** - Validated against the official cel-spec test suite

### Language Server
- **Syntax Highlighting** - Semantic tokens for accurate code coloring
- **Error Diagnostics** - Real-time parsing and type checking errors
- **Hover Information** - Type and function documentation on hover
- **Protovalidate Support** - CEL validation in `.proto` files

## Installation

Add `cel-core` to your `Cargo.toml`:

```toml
[dependencies]
cel-core = "0.1"

# Optional: for proto wire format interop with cel-go/cel-cpp
cel-core-proto = "0.1"
```

## Usage

### Basic Usage with cel-core

```rust
use cel_core::{Env, CelType};

// Create an environment with the standard library
let env = Env::with_standard_library()
    .with_variable("user_age", CelType::Int)
    .with_variable("user_name", CelType::String);

// Parse and type-check an expression
let ast = env.compile("user_age >= 18 && user_name.startsWith('admin')")?;

// Access type information
assert!(ast.is_checked());
assert_eq!(ast.result_type(), Some(&CelType::Bool));

// Convert back to CEL source
println!("{}", ast.to_cel_string());
```

### Extensions

```rust
use cel_core::{Env, CelType};
use cel_core::ext::{StringsExtension, MathExtension};

// Enable specific extensions
let env = Env::with_standard_library()
    .with_extension(StringsExtension)
    .with_extension(MathExtension)
    .with_variable("values", CelType::list(CelType::Int));

// Use extension functions
let ast = env.compile("math.greatest(values)")?;
```

### Proto Wire Format (Interop with cel-go/cel-cpp)

For wire compatibility with other CEL implementations, use `cel-core-proto`:

```rust
use cel_core::{Env, CelType};
use cel_core_proto::{to_parsed_expr, to_checked_expr};

let env = Env::with_standard_library()
    .with_variable("x", CelType::Int);

let ast = env.compile("x + 1")?;

// Convert to proto format for serialization/interop
let parsed_expr = to_parsed_expr(ast.expr(), ast.source());
let checked_expr = to_checked_expr(ast.type_info().unwrap(), &parsed_expr);

// These can be serialized with prost and sent to cel-go/cel-cpp
```

### Low-level Parser API

```rust
use cel_core::{parse, Expr};

let result = parse("x + y > 10");

// Check for errors (parser supports error recovery)
if !result.errors.is_empty() {
    for error in &result.errors {
        println!("Error: {} at {:?}", error.message, error.span);
    }
}

// AST may exist even with errors due to error recovery
if let Some(ast) = result.ast {
    match &ast.node {
        Expr::Binary { op, left, right } => {
            println!("Binary operation: {:?}", op);
        }
        _ => {}
    }
}
```

## Crate Structure

This project is organized as a Cargo workspace:

| Crate | Description |
|-------|-------------|
| **cel-core** | Main CEL library with parser, type checker, and standard library. This is the primary public API. |
| **cel-core-proto** | Protobuf types and bidirectional conversion between AST and cel-spec proto format. Enables wire compatibility with cel-go/cel-cpp. |
| **cel-core-lsp** | Language Server Protocol implementation using `tower-lsp`. Provides diagnostics, hover, and semantic tokens for IDE integration. |
| **cel-core-conformance** | Conformance testing against the official [cel-spec](https://github.com/google/cel-spec) test suite. Not published to crates.io. |

```
crates/
  cel-core/              # Main library (parser, checker, types, extensions)
  cel-core-proto/        # Proto wire format conversion
  cel-core-lsp/          # Language Server Protocol implementation
  cel-core-conformance/  # Conformance testing against cel-spec
```

## Roadmap

CEL-Core is working toward full CEL implementation parity with cel-go. See [ROADMAP.md](ROADMAP.md) for the detailed implementation plan.

### Current Status

| Component | Status | Notes |
|-----------|--------|-------|
| **Lexer** | Complete | Logos-based, all CEL tokens |
| **Parser** | Complete | Recursive descent, error recovery, macros |
| **Type Checker** | Complete | Type inference, overload resolution |
| **Standard Library** | Complete | All standard functions and operators |
| **Extensions** | Complete | Strings, math, encoders, optionals |
| **Proto Conversion** | Complete | `ParsedExpr` and `CheckedExpr` |
| **LSP** | Partial | Diagnostics, hover, semantic tokens |
| **Evaluation** | Not started | No runtime execution yet |

### Milestones

1. ~~**Parser** - Full CEL syntax with error recovery~~
2. ~~**Type Checker** - Type inference and `CheckedExpr` production~~
3. ~~**Extensions** - String, math, encoder, and optional extensions~~
4. **Evaluation** - Runtime execution with variable bindings
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
