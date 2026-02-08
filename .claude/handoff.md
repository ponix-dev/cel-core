# Roadmap Handoff

## Last Updated
2026-02-08

## Just Completed
- GitHub Issue: #59 — Clean up public API surface and export patterns
- [x] Made `checker`, `eval`, `unparser` modules private in cel-core
- [x] Removed internal types from root re-exports (`check()`, `STANDARD_LIBRARY`, `Evaluator`, `FunctionRegistry`, etc.)
- [x] Made lexer types (`lex()`, `Token`, `LexError`, `SpannedToken`) `pub(crate)` in parser
- [x] Made `message` and `registry` modules private in cel-core-proto, kept root re-exports
- [x] Made `AstConverter`, operator mappings, source_info helpers, and WKT helpers `pub(crate)` in cel-core-proto
- [x] Made `types`, `protovalidate`, `settings` modules `pub(crate)` in cel-core-lsp
- [x] Made `Backend` struct `pub(crate)`, only `create_service()` is public
- [x] Adopted consistent pattern: private modules + selective root re-export
- [x] Moved test code into `#[cfg(test)] mod tests` modules
- [x] Bundled imports and fixed dead code warnings
- [x] Added `cargo fmt` to CLAUDE.md development commands

### Summary
Major API cleanup across all three crates. Tightened public exports by making internal modules private and removing implementation details from the public API surface. Adopted a consistent "private modules + selective root re-export" pattern. The public API is now minimal and intentional: `cel-core` exposes `Env`, `Ast`, types, parsing, and extension traits; `cel-core-proto` exposes registry, message, and conversion functions; `cel-core-lsp` only exposes `create_service()`.

### Key files modified
- `crates/cel-core/src/lib.rs` — tightened re-exports, made modules private
- `crates/cel-core/src/checker/` — made `pub(crate)`, restructured exports
- `crates/cel-core/src/eval/` — made `pub(crate)`, restructured exports
- `crates/cel-core/src/parser/` — lexer types made `pub(crate)`
- `crates/cel-core-proto/src/lib.rs` — tightened re-exports, made modules private
- `crates/cel-core-lsp/src/lib.rs` — made internal modules `pub(crate)`
- `crates/cel-core-lsp/src/types/builtins.rs` — removed redundant LSP builtins (now from checker)
- All example files updated to use new public API paths

### Notable decisions
- Kept `ext` module public in cel-core since extension libraries are user-facing
- Kept `parser` module public but only exports `parse()`, `ParseResult`, and macro types
- `types` module remains public for `CelType`, `CelValue`, `Expr`, etc.
- Conformance crate needed direct access to checker/eval internals, handled via targeted `pub(crate)` within the workspace

## Previous Work
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #58 — Macro calls with wrong argument count produce misleading 'undeclared reference' error
  - Parser/checker bug where bad macro arity gives confusing error messages
  - Should be a focused fix in the macro expansion or checker error reporting
- GitHub Issue: #38 — Richer hover information using CheckResult.type_map
  - Now that the LSP uses the checker, hover can show precise types from the type map
  - Natural follow-up since the infrastructure is already in place
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
  - Clean separation of concerns, consolidate proto conversion in one place

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but root cause may need investigation.
- Completion for map types could be improved — currently only shows methods, not key access patterns
- Settings discovery could be enhanced to walk up directory tree (currently only checks workspace root)
