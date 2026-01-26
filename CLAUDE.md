# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CEL-Core is a Language Server Protocol implementation for the Common Expression Language (CEL). It provides IDE support including syntax highlighting, error diagnostics, hover information, and semantic tokens for CEL expressions.

## Development Commands

### Building
```bash
# Debug build
cargo build

# Release build
cargo build --release

# Build specific crate
cargo build -p cel-core
```

### Testing
```bash
# Run all tests (excludes conformance tests by default)
mise run test

# Run unit tests only
mise run test:unit

# Run integration tests
mise run test:integration
```

### Conformance Testing
```bash
# Set up cel-spec submodule (one-time after clone)
mise run conformance:setup

# Pull latest cel-spec test data
mise run conformance:update

# Run CEL conformance tests
mise run conformance:test
```

### Protobuf Code Generation
```bash
# Generate Rust code from cel-spec protobuf definitions
mise run proto:generate
```

### Installing
```bash
# Install cel-core-lsp binary to PATH
mise run install
```

### First-time Setup

After cloning the repository, initialize the git submodule for CEL conformance tests:

```bash
git submodule update --init --recursive
```

This fetches the google/cel-spec repository required for running official conformance tests.

## Architecture

### Workspace Structure

This is a multi-crate Cargo workspace:

- **`cel-core`**: Main CEL library (primary public API)
  - Unified `Env` builder for configuring CEL environments
  - `Ast` wrapper for parsed/checked expressions
  - Internal modules:
    - `types/` - CelType, CelValue, AST types, declarations
    - `parser/` - Lexer (using `logos`), recursive descent parser, macro expansion
    - `checker/` - Type checker, overload resolution, standard library
    - `ext/` - Extension libraries (strings, math, encoders, optionals)
  - Re-exports common types for convenience

- **`cel-core-proto`**: Protobuf types and conversion (optional)
  - Generated protobuf types from cel-spec
  - Bidirectional conversion between AST and proto format
  - Wire compatibility with other CEL implementations (cel-go, cel-cpp)

- **`cel-core-lsp`**: Language Server Protocol implementation (binary)
  - Uses `tower-lsp` for LSP protocol
  - Internal module structure:
    - `document/` - Document state management and text utilities
      - `text.rs` - `LineIndex` for byte offset ↔ LSP position conversion
      - `region.rs` - `CelRegion`, `OffsetMapper` for embedded CEL in proto files
      - `state.rs` - `DocumentState`, `DocumentStore` for document lifecycle
    - `lsp/` - LSP protocol feature implementations
      - `diagnostics.rs` - Error to LSP diagnostic conversion
      - `hover.rs` - Hover information for CEL expressions
      - `semantic_tokens.rs` - Syntax highlighting tokens
    - `types/` - CEL type system and validation
      - `builtins.rs` - Built-in function definitions
      - `validation.rs` - Semantic validation (undefined vars, arity, types)
    - `protovalidate/` - Protovalidate CEL extension support
      - `builtins.rs` - Protovalidate-specific functions (isEmail, isUri, etc.)
      - `proto_parser.rs` - Extract CEL from .proto files
      - `resolver.rs` - Variable resolver for protovalidate context

- **`cel-core-conformance`**: CEL conformance testing (not published)
  - In-process `ConformanceService` trait (not gRPC)
  - Tests parser against official CEL spec test suite (.textproto files)
  - cel-spec git submodule for test data and proto files
  - Internal module structure:
    - `loader.rs` - Load .textproto test files using prost-reflect
    - `service.rs` - `CelConformanceService` implementation

## Protobuf Code Generation

The project uses buf to generate Rust code from the cel-spec protobuf definitions.

### Configuration Files

- `buf.yaml` - Defines the protobuf module location
- `buf.gen.yaml` - Configures the neoeinstein-prost plugin for code generation

### Regenerating Code

To regenerate the protobuf types after updating cel-spec:

```bash
mise run proto:generate
```

This generates code into `crates/cel-core-proto/src/gen/`.

## CEL API

### High-level API (Recommended)

```rust
use cel_core::{Env, CelType};

// Create environment with standard library
let env = Env::with_standard_library()
    .with_variable("x", CelType::Int)
    .with_variable("name", CelType::String);

// Parse and type-check
let ast = env.compile("x > 10 && name.startsWith('test')")?;

// Access type information
assert_eq!(ast.result_type(), Some(&CelType::Bool));
```

### Proto Conversion (for interop with cel-go/cel-cpp)

```rust
use cel_core::{Env, CelType};
use cel_core_proto::{to_parsed_expr, to_checked_expr};

let env = Env::with_standard_library()
    .with_variable("x", CelType::Int);

let ast = env.compile("x + 1")?;

// Convert to proto format for wire compatibility
let parsed = to_parsed_expr(ast.expr(), ast.source());
let checked = to_checked_expr(ast.type_info().unwrap(), &parsed);
```

### Low-level Parser API

```rust
use cel_core::{parse, ParseResult, types::Expr, SpannedExpr};

let result: ParseResult = parse("x + y > 10");

// Check for errors
if result.is_err() {
    for error in &result.errors {
        println!("{} at {:?}", error.message, error.span);
    }
}

// Process AST (may exist even with errors due to error recovery)
if let Some(ast) = result.ast {
    // ast.node: Expr - the expression type
    // ast.span: Range<usize> - byte offsets in source
}
```

## Code Conventions

### Commits
- Use conventional commit format: `type: short description`
- Types: `feat`, `fix`, `refactor`, `test`, `docs`, `chore`
- Keep commit messages to a single sentence
- Do NOT add Claude co-authorship to commits
- Save detailed explanations for PR descriptions

### Error Handling
- Parser supports error recovery - `ParseResult` can have both `ast` and `errors`
- `Expr::Error` nodes mark unparseable sections in partial ASTs
- Errors include span information for diagnostic display

### Testing
- Unit tests in each crate's `src/` directory
- Integration tests in `crates/cel-core/tests/`
- Conformance tests in `crates/cel-core-conformance/`
- Run `mise run test` for all tests (excluding conformance)
- Run `mise run conformance:test` for CEL spec conformance tests
