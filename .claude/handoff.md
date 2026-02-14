# Roadmap Handoff

## Last Updated
2026-02-14

## Just Completed
- GitHub Issue: #66 — Show variable types on hover
- [x] Updated `hover_for_node` and `hover_at_position` to accept `Option<&CheckResult>` instead of `&[CheckError]`
- [x] Added variable type hover for `Ident` and `RootIdent` nodes via `CheckResult.type_map`
- [x] Threaded check result through proto hover path (`hover_at_position_proto`)
- [x] Added tests for variable type hover (int variable, message type)

### Summary
Hovering over variables in CEL expressions now shows their declared type (e.g., `(variable) x: int`). The type information was already available in `CheckResult.type_map` — this change threads the full `CheckResult` to the hover functions and looks up identifier nodes by their AST node ID. Works for both standalone `.cel` files and embedded CEL in `.proto` files.

### Key files
- **Modified:** `crates/cel-core-lsp/src/lsp/hover.rs` — signature changes, variable type hover logic, new tests
- **Modified:** `crates/cel-core-lsp/src/lib.rs` — pass `check_result.as_ref()` instead of `check_errors()`

### Notable decisions
- Variable hover displays as `` (variable) `name`: `type` `` using markdown inline code for readability
- Check errors take priority over variable type display (undeclared reference error shown instead of type)
- Variable type hover is inserted between error check and builtin docs fallback in the priority chain

## Previous Work
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
- GitHub Issue: #58 — Macro calls with wrong argument count produce misleading 'undeclared reference' error
- GitHub Issue: #60 — Automate releases with GitHub Actions
- GitHub Issue: #59 — Clean up public API surface and export patterns
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #65 — Walk file tree to discover settings.toml
  - Settings discovery enhanced to walk up directory tree
- GitHub Issue: #38 — Richer hover information using CheckResult.type_map
  - Now partially addressed by #66; remaining work could include showing function return types, expression types, etc.

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but root cause may need investigation.
- Completion for map types could be improved — currently only shows methods, not key access patterns
