# CEL-Core

A complete [Common Expression Language](https://github.com/google/cel-spec) (CEL) implementation in Rust, validated through the official cel-spec conformance test suite.

CEL-Core provides the full CEL processing pipeline—parsing, type checking, and proto wire format conversion—with the goal of achieving implementation parity with [cel-go](https://github.com/google/cel-go), the reference implementation.

> **Note:** This project is a work in progress. See the [Roadmap](ROADMAP.md) for implementation status and the [conformance tests](crates/cel-core-conformance) for cel-spec compatibility.

Built on top of the core implementation is a Language Server Protocol (LSP) server that brings CEL support to any LSP-compatible editor.

## Features

### CEL Implementation
- **Parser** - Full CEL syntax support with error recovery
- **Type Checker** - Type inference, overload resolution, and validation
- **Evaluator** - Runtime expression evaluation with variable bindings
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

### Full Workflow: Parse, Check, and Evaluate

```rust
use cel_core::{Env, CelType, Value, MapActivation};

// 1. Create an environment with the standard library and declare variables
let env = Env::with_standard_library()
    .with_variable("user", CelType::String)
    .with_variable("age", CelType::Int);

// 2. Compile the expression (parse + type-check)
let ast = env.compile("age >= 21 && user.startsWith('admin')")?;

// 3. Create a program from the compiled AST
let program = env.program(&ast)?;

// 4. Set up variable bindings for evaluation
let mut activation = MapActivation::new();
activation.insert("user", Value::String("admin_alice".into()));
activation.insert("age", Value::Int(25));

// 5. Evaluate the expression
let result = program.eval(&activation);
assert_eq!(result, Value::Bool(true));
```

### Working with Different Value Types

```rust
use cel_core::{Env, CelType, Value, MapActivation};
use std::sync::Arc;

let env = Env::with_standard_library()
    .with_variable("numbers", CelType::list(CelType::Int))
    .with_variable("metadata", CelType::map(CelType::String, CelType::Int));

let ast = env.compile("size(numbers) > 0 && metadata['count'] == 42")?;
let program = env.program(&ast)?;

let mut activation = MapActivation::new();

// List values
activation.insert("numbers", Value::List(Arc::from(vec![
    Value::Int(1),
    Value::Int(2),
    Value::Int(3),
])));

// Map values
use cel_core::eval::{ValueMap, MapKey};
let mut map = ValueMap::new();
map.insert(MapKey::String("count".into()), Value::Int(42));
activation.insert("metadata", Value::Map(Arc::new(map)));

let result = program.eval(&activation);
assert_eq!(result, Value::Bool(true));
```

### Extensions

```rust
use cel_core::{Env, CelType, Value, MapActivation};

// Enable all extensions
let env = Env::with_standard_library()
    .with_all_extensions()
    .with_variable("values", CelType::list(CelType::Int));

let ast = env.compile("math.greatest(values)")?;
let program = env.program(&ast)?;

let mut activation = MapActivation::new();
activation.insert("values", Value::List(std::sync::Arc::from(vec![
    Value::Int(10),
    Value::Int(42),
    Value::Int(7),
])));

let result = program.eval(&activation);
assert_eq!(result, Value::Int(42));
```

### Error Handling

```rust
use cel_core::{Env, CelType, Value, MapActivation};

let env = Env::with_standard_library()
    .with_variable("x", CelType::Int);

let ast = env.compile("10 / x")?;
let program = env.program(&ast)?;

let mut activation = MapActivation::new();
activation.insert("x", Value::Int(0));

let result = program.eval(&activation);

// Division by zero returns an error value
match result {
    Value::Error(err) => println!("Evaluation error: {}", err),
    other => println!("Result: {:?}", other),
}
```

### Proto Wire Format (Interop with cel-go/cel-cpp)

For wire compatibility with other CEL implementations, use `cel-core-proto`:

```rust
use cel_core::{Env, CelType};
use cel_core_proto::AstToProto;
use prost::Message;

let env = Env::with_standard_library()
    .with_variable("x", CelType::Int);

let ast = env.compile("x + 1")?;

// Convert to proto format
let parsed_expr = ast.to_parsed_expr();
let checked_expr = ast.to_checked_expr()?;

// Serialize to bytes with prost (wire-compatible with cel-go/cel-cpp)
let parsed_bytes = parsed_expr.encode_to_vec();
let checked_bytes = checked_expr.encode_to_vec();

// Deserialize from bytes
use cel_core_proto::{ParsedExpr, CheckedExpr};
let decoded_parsed = ParsedExpr::decode(parsed_bytes.as_slice())?;
let decoded_checked = CheckedExpr::decode(checked_bytes.as_slice())?;
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
| **Evaluator** | Partial | Core evaluation working, see conformance status |
| **Standard Library** | Complete | All standard functions and operators |
| **Extensions** | Partial | Strings, math, encoders, optionals (eval in progress) |
| **Proto Conversion** | Complete | `ParsedExpr` and `CheckedExpr` |
| **LSP** | Partial | Diagnostics, hover, semantic tokens |

### Conformance Testing

CEL-Core is validated against the official [cel-spec](https://github.com/google/cel-spec) conformance test suite:

| Test Category | Status |
|--------------|--------|
| integer_math | ✅ 100% |
| fp_math | ✅ 100% |
| basic | 98% |
| string | 98% |
| logic | 93% |
| macros | 95% |
| comparisons | 76% |
| lists | 79% |
| Proto messages | In progress |
| Timestamps/Durations | In progress |

### Milestones

1. ~~**Parser** - Full CEL syntax with error recovery~~
2. ~~**Type Checker** - Type inference and `CheckedExpr` production~~
3. ~~**Extensions** - String, math, encoder, and optional extensions~~
4. ~~**Evaluation** - Runtime execution with variable bindings~~
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
