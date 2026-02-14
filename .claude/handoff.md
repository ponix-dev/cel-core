# Roadmap Handoff

## Last Updated
2026-02-14

## Just Completed
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
- [x] Created `crates/cel-core-proto/src/value_conversion.rs` with four public conversion functions
- [x] Registered module and re-exports in `crates/cel-core-proto/src/lib.rs`
- [x] Updated `crates/cel-core-conformance/src/service.rs` to import from cel-core-proto
- [x] Added unit tests for all value types and function declaration conversion

### Summary
Moved four proto↔CEL value conversion functions from the conformance crate to cel-core-proto, completing the proto interop story. Any user doing gRPC integration or working with serialized evaluation contexts can now use `proto_value_to_value`, `value_to_proto_value`, `value_to_expr_value`, and `function_decl_from_proto` directly from cel-core-proto. The conformance service now imports these instead of defining its own copies.

### Key files
- **Created:** `crates/cel-core-proto/src/value_conversion.rs` — new module with 4 public functions + tests
- **Modified:** `crates/cel-core-proto/src/lib.rs` — added module registration and re-exports
- **Modified:** `crates/cel-core-conformance/src/service.rs` — removed 4 functions, updated imports/call sites

### Notable decisions
- `function_decl_from_proto` takes `(name: &str, overloads: &[Overload])` using proto types directly, avoiding dependency on conformance-local wrapper types
- `bindings_to_activation` kept in conformance since it depends on the conformance-local `Binding` type
- Renamed `value_to_proto` → `value_to_expr_value` and `convert_function_decl` → `function_decl_from_proto` for clarity

## Previous Work
- GitHub Issue: #58 — Macro calls with wrong argument count produce misleading 'undeclared reference' error
- GitHub Issue: #60 — Automate releases with GitHub Actions
- GitHub Issue: #59 — Clean up public API surface and export patterns
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #66 — Show variable types on hover
  - Richer hover information using CheckResult.type_map
  - Natural follow-up since the LSP already uses the checker
- GitHub Issue: #65 — Walk file tree to discover settings.toml
  - Settings discovery enhanced to walk up directory tree

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but root cause may need investigation.
- Completion for map types could be improved — currently only shows methods, not key access patterns
