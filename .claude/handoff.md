# Roadmap Handoff

## Last Updated
2026-02-02

## Just Completed
- GitHub Issue: #29 (5.10: cel.block Extension)
- [x] cel.block macro (nested Bind expansion)
- [x] cel.index macro
- [x] cel.iterVar macro
- [x] cel.accuVar macro
- [x] Fix checker: unwrap optional values in optional map entries

### Summary
Implemented the `cel.block` extension which provides slot-based variable binding for common subexpression elimination. Added four test-only macros (`cel.block`, `cel.index`, `cel.iterVar`, `cel.accuVar`) that expand at parse time. `cel.block` expands into nested `Expr::Bind` nodes, reusing existing bind infrastructure. Also fixed a checker bug where optional map entries (`{?"key": value}`) weren't unwrapping the optional from the value type.

### Key files modified
- `crates/cel-core/src/parser/macros.rs` — Added 4 new macros + registered in STANDARD_MACROS
- `crates/cel-core/src/checker/checker.rs` — Fixed `check_map` to unwrap optional value types for optional entries
- `.claude/context/conformance-baseline.md` — Updated baseline (block_ext 0→37/37)

### Notable decisions
- `cel.block` expands to nested `Expr::Bind` nodes rather than a new AST variant — simpler, reuses existing checker/evaluator support
- Eager evaluation (all slots evaluated even if unused) vs cel-go's lazy approach — correct for conformance, may optimize later
- Code comment documents the future optimization path (flat `Vec<Option<Value>>` + lazy eval)

### Results
- block_ext.textproto parse_check: 0/37 → **37/37** (+37)
- block_ext.textproto eval: 0/37 → **37/37** (+37)
- Overall: +74 conformance tests, no regressions

## Next Up
- GitHub Issue: #50 — Decouple prost/prost-reflect from cel-core via trait abstraction
  - Extract a trait-based interface so cel-core doesn't depend directly on prost/prost-reflect
  - Enables alternative protobuf backends and cleaner dependency boundaries
- GitHub Issue: #52 — Refactor: break down evaluator.rs into focused submodules
  - Split the monolithic evaluator into smaller, focused modules
  - Improve maintainability and readability of evaluation logic

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — may revisit if additional test cases require it.
