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

### Quick Start

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
activation.insert("user", "admin_alice");  // &str converts automatically
activation.insert("age", 25);              // integers widen automatically

// 5. Evaluate the expression
let result = program.eval(&activation);
assert_eq!(result, Value::Bool(true));
```

> See [examples/quickstart.rs](crates/cel-core/examples/quickstart.rs) for the complete example.

### Creating Values

`Value` implements `From` for idiomatic Rust conversions. Integers automatically widen to CEL's `int` (i64) or `uint` (u64):

```rust
use cel_core::Value;

// Primitives - integers widen automatically
let v: Value = 42.into();             // i32 -> Value::Int(42)
let v: Value = true.into();           // Value::Bool(true)
let v: Value = "hello".into();        // Value::String(...)
let v: Value = 3.14.into();           // Value::Double(3.14)

// Bytes from Vec<u8>
let v: Value = vec![1u8, 2, 3].into(); // Value::Bytes(...)

// Lists using Value::list() - items convert automatically
let v = Value::list([1, 2, 3]);        // list of ints
let v = Value::list(["a", "b", "c"]);  // list of strings

// Maps using Value::map() - keys and values convert automatically
let v = Value::map([("host", "localhost"), ("port", "8080")]);

// Mixed value types require explicit Value::from()
let v = Value::map([
    ("name", Value::from("Alice")),
    ("age", Value::from(30)),
    ("active", Value::from(true)),
]);
```

### Extracting Values

Use `TryFrom` to extract Rust types from `Value`:

```rust
use cel_core::{Value, ValueError};

let result = Value::Int(42);
let n: i64 = (&result).try_into().unwrap();

let result: Value = "hello".into();
let s: &str = (&result).try_into().unwrap();

// Handle type mismatches
let result: Value = "not a number".into();
let attempt: Result<i64, ValueError> = (&result).try_into();
assert!(attempt.is_err());
```

> See [examples/extract_values.rs](crates/cel-core/examples/extract_values.rs) for more extraction examples.

### Lists

```rust
use cel_core::{Env, CelType, Value, MapActivation};

let env = Env::with_standard_library()
    .with_variable("numbers", CelType::list(CelType::Int));

let mut activation = MapActivation::new();
activation.insert("numbers", Value::list([1, 5, 3, 8, 2]));

// Filter, map, exists, all - CEL list macros
let ast = env.compile("numbers.filter(x, x > 3)").unwrap();
let program = env.program(&ast).unwrap();
println!("{}", program.eval(&activation));  // [5, 8]

let ast = env.compile("numbers.map(x, x * 2)").unwrap();
let program = env.program(&ast).unwrap();
println!("{}", program.eval(&activation));  // [2, 10, 6, 16, 4]
```

> See [examples/lists.rs](crates/cel-core/examples/lists.rs) for more list operations.

### Maps

```rust
use cel_core::{Env, CelType, Value, MapActivation};

let env = Env::with_standard_library()
    .with_variable("user", CelType::map(CelType::String, CelType::Dyn));

// Mixed value types require explicit Value::from()
let user = Value::map([
    ("name", Value::from("Alice")),
    ("age", Value::from(30)),
    ("active", Value::from(true)),
]);

let mut activation = MapActivation::new();
activation.insert("user", user);

// Field access and index access
let ast = env.compile("user.name").unwrap();         // "Alice"
let ast = env.compile(r#"user["age"]"#).unwrap();    // 30
let ast = env.compile(r#""name" in user"#).unwrap(); // true
let ast = env.compile("has(user.email)").unwrap();   // false
```

> See [examples/maps.rs](crates/cel-core/examples/maps.rs) for more map operations.

### Extensions

Extensions provide additional functions beyond the standard library. Currently, extensions are available for type checking; runtime evaluation is in progress.

```rust
use cel_core::{Env, CelType};

// Enable all extensions (strings, math, encoders, optionals)
let env = Env::with_standard_library()
    .with_all_extensions()
    .with_variable("values", CelType::list(CelType::Int))
    .with_variable("text", CelType::String);

// Math extension - type checks correctly
let ast = env.compile("math.greatest(values)")?;
let ast = env.compile("math.least(values)")?;

// String extension - type checks correctly
let ast = env.compile("text.split(' ')")?;
let ast = env.compile("['a', 'b'].join('-')")?;
```

> See [examples/extensions.rs](crates/cel-core/examples/extensions.rs) for more details.

### Error Handling

CEL returns error values for runtime errors like division by zero or index out of bounds:

```rust
use cel_core::{Env, CelType, Value, MapActivation};

let env = Env::with_standard_library()
    .with_variable("x", CelType::Int);

let ast = env.compile("10 / x")?;
let program = env.program(&ast)?;

let mut activation = MapActivation::new();
activation.insert("x", 0);

let result = program.eval(&activation);

// Division by zero returns an error value
match result {
    Value::Error(err) => println!("Error: {}", err),
    other => println!("Result: {}", other),
}

// Use has() to safely check field existence before access
let ast = env.compile("has(config.key) ? config.key : 'default'")?;
```

> See [examples/error_handling.rs](crates/cel-core/examples/error_handling.rs) for more error handling patterns.

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
