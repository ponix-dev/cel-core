# Roadmap Handoff

## Last Updated
2026-02-08

## Just Completed
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- [x] Replaced custom LSP type checker (`types/checker.rs`, `types/validation.rs`) with cel-core's unified checker
- [x] Added `settings.toml` configuration system for LSP workspaces (variables, extensions, proto file descriptors)
- [x] Integrated cel-core-proto into the LSP for proto type resolution via file descriptor sets
- [x] Added completion support using placeholder-based approach with checker type information
- [x] Added protovalidate `this` context support for field-level CEL expressions
- [x] Added snapshot-based integration tests using `expect-test`
- [x] Updated diagnostics, hover, and semantic tokens to use checker AST/type information

### Summary
Major refactor replacing ~1900 lines of custom validation/checker code with cel-core's checker. The LSP now uses `Env::compile()` for type checking, getting accurate type information, overload resolution, and error diagnostics from the same checker used at runtime. Added a `settings.toml` configuration system so workspaces can declare variables, enable extensions, and point to proto file descriptor sets.

### Key files added/modified
- `crates/cel-core-lsp/src/settings.rs` — new settings.toml parser and workspace configuration
- `crates/cel-core-lsp/src/lsp/completion.rs` — new completion provider using checker types
- `crates/cel-core-lsp/src/lsp/diagnostics.rs` — updated to use checker errors
- `crates/cel-core-lsp/src/lsp/hover.rs` — updated to use checker type map
- `crates/cel-core-lsp/src/lsp/semantic_tokens.rs` — updated to use checker AST
- `crates/cel-core-lsp/src/document/state.rs` — stores `Arc<Env>` for re-parsing
- `crates/cel-core-lsp/src/document/region.rs` — proto region state with env
- `crates/cel-core-lsp/src/protovalidate/proto_parser.rs` — enhanced `this` context and has() support
- `crates/cel-core-lsp/src/protovalidate/resolver.rs` — updated for checker integration
- `crates/cel-core-lsp/src/types/checker.rs` — **deleted** (replaced by cel-core checker)
- `crates/cel-core-lsp/src/types/validation.rs` — **deleted** (replaced by cel-core checker)
- `crates/cel-core/src/env.rs` — added `methods_for_type()`, `standalone_functions()` for completion
- `crates/cel-core/src/checker/checker.rs` — enhanced for LSP use cases
- `crates/cel-core-proto/src/registry.rs` — added `message_field_names()` for completion

### Notable decisions
- Used placeholder-based completion: inserts `__cel_complete__` at cursor position, re-parses and type-checks, then finds the placeholder in the AST to determine context
- Settings use TOML format with workspace-relative paths for proto file descriptors
- Proto file descriptor sets are loaded as binary `.binpb` files (output of `buf build`)

## Previous Work
- Refactored trait abstractions: split `TypeRegistry` into `ProtoTypeResolver` + `ProtoRegistry`
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #38 — Richer hover information using CheckResult.type_map
  - Now that the LSP uses the checker, hover can show precise types from the type map
  - Natural follow-up since the infrastructure is already in place
- GitHub Issue: #42 — Go-to-definition and find references
  - Checker AST has reference information that could power navigation features
- GitHub Issue: #44 — LSP workspace/configuration support
  - Extend settings.toml with more configuration options

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but root cause may need investigation.
- Completion for map types could be improved — currently only shows methods, not key access patterns
- Settings discovery could be enhanced to walk up directory tree (currently only checks workspace root)
