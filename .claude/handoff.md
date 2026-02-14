# Roadmap Handoff

## Last Updated
2026-02-13

## Just Completed
- GitHub Issue: #58 — Macro calls with wrong argument count produce misleading 'undeclared reference' error
- [x] Added `MacroRegistry::expected_args_description()` for human-readable arity descriptions
- [x] Fixed `try_macro_expansion` to return `Expr::Error` on arity mismatch instead of falling back to `Expr::Call`
- [x] Fixed `MacroExpansion::Error` path to return `Expr::Error` instead of `None`
- [x] Macro errors now included in `ParseResult.errors` (previously discarded)
- [x] Added 6 new tests covering all error paths, updated 1 existing test

### Summary
When a macro like `has()` was called with wrong arguments, the parser silently fell back to a `Call` node, causing the checker to report a misleading "undeclared reference to 'has'" error. Now the parser detects the arity mismatch at the macro level, produces an `Expr::Error` node (which the checker silently skips), and surfaces the actual error message (e.g., "has() requires exactly 1 argument, got 0") through the normal parse error path.

### Key files modified
- `crates/cel-core/src/parser/macros.rs` — added `expected_args_description()` method
- `crates/cel-core/src/parser/parse.rs` — fixed `try_macro_expansion`, surfaced macro errors, added tests

### Notable decisions
- `Expr::Error` nodes are already handled by the checker (returns `CelType::Error` silently), so no checker changes needed
- Macro errors flow through the existing `ParseResult.errors` → LSP diagnostics pipeline
- Two distinct failure paths both now handled: arity mismatch (no matching overload) and expansion failure (e.g., `has(1+2)` where arg isn't a field selection)

## Previous Work
- GitHub Issue: #60 — Automate releases with GitHub Actions
- GitHub Issue: #59 — Clean up public API surface and export patterns
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #38 — Richer hover information using CheckResult.type_map
  - Now that the LSP uses the checker, hover can show precise types from the type map
  - Natural follow-up since the infrastructure is already in place
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
  - Clean separation of concerns, consolidate proto conversion in one place

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but root cause may need investigation.
- Completion for map types could be improved — currently only shows methods, not key access patterns
- Settings discovery could be enhanced to walk up directory tree (currently only checks workspace root)
