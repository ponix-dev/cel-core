# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CEL-LSP is a Language Server Protocol implementation for the Common Expression Language (CEL). It provides IDE support including syntax highlighting, error diagnostics, hover information, and semantic tokens for CEL expressions.

## Development Commands

### Building
```bash
# Debug build
cargo build

# Release build
cargo build --release

# Build specific crate
cargo build -p cel-parser
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

# Run CEL conformance tests (requires BSR authentication)
mise run conformance:test
```

### Installing
```bash
# Install cel-lsp binary to PATH
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

- **`cel-parser`**: Core CEL expression parser
  - Lexer using `logos` crate
  - Hand-written recursive descent parser
  - AST types with span tracking for diagnostics
  - Error recovery with partial AST construction

- **`cel-lsp`**: Language Server Protocol implementation
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

- **`cel-conformance`**: CEL conformance testing
  - In-process `ConformanceService` trait (not gRPC)
  - Tests parser against official CEL spec test suite (.textproto files)
  - Uses Buf Schema Registry for proto types
  - cel-spec git submodule for test data files
  - Internal module structure:
    - `loader.rs` - Load .textproto test files using prost-reflect
    - `service.rs` - `CelConformanceService` implementation

## Buf Schema Registry (BSR) Setup

The `cel-conformance` crate uses pre-generated protobuf types from the Buf Schema Registry.

### Registry Configuration

The `.cargo/config.toml` configures the buf registry:

```toml
[registries.buf]
index = "sparse+https://buf.build/gen/cargo/"
credential-provider = "cargo:token"
```

### Authentication

**One-time setup required per machine:**

1. Get a BSR token from https://buf.build/settings/user
2. Login to the buf registry:
   ```bash
   cargo login --registry buf "Bearer $BSR_TOKEN"
   ```

This stores credentials in `~/.cargo/credentials.toml`.

### BSR Packages Used

The project uses these pre-generated SDK packages from BSR:

- `google_cel-spec_community_neoeinstein-prost` - Prost message types for CEL spec

Package naming convention: `{owner}_{module}_community_{plugin}`

### Updating Proto Dependencies

To update to a newer version of the CEL spec protos:

1. Check available versions at https://buf.build/google/cel-spec/sdks
2. Update version in `crates/cel-conformance/Cargo.toml`
3. Run `cargo update -p google_cel-spec_community_neoeinstein-prost`

## Parser API

The parser provides a simple API:

```rust
use cel_parser::{parse, ParseResult, Expr, SpannedExpr};

let result: ParseResult = cel_parser::parse("x + y > 10");

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

### Error Handling
- Parser supports error recovery - `ParseResult` can have both `ast` and `errors`
- `Expr::Error` nodes mark unparseable sections in partial ASTs
- Errors include span information for diagnostic display

### Testing
- Parser tests in `crates/cel-parser/tests/`
- Use `assert_parses()` helper for successful parse tests
- Use `assert_parse_error()` helper for error case tests
