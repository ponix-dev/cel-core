# Roadmap Handoff

## Last Updated
2026-02-02

## Just Completed
- GitHub Issue: #52 — Refactor: break down evaluator.rs into focused submodules
- [x] Extract `proto.rs` — proto-to-CEL/CEL-to-proto conversions, struct construction, WKT/Any handling
- [x] Extract `conversions.rs` — type conversion dispatcher and all `convert_to_*` functions
- [x] Extract `operators.rs` — arithmetic, comparison, and unary operators
- [x] Extract `builtins.rs` — built-in function dispatch and timestamp accessors
- [x] Extract `resolve.rs` — identifier resolution, container/scope logic
- [x] Update mod.rs to register new submodules

### Summary
Split the monolithic `evaluator.rs` (3,505 lines) into 5 focused submodules plus a slimmed-down core (1,349 lines). Each submodule contains `impl Evaluator<'a>` blocks in separate files within `crates/cel-core/src/eval/`. Methods called cross-module use `pub(super)` visibility. No public API changes — only internal file reorganization.

### Key files added/modified
- `crates/cel-core/src/eval/proto.rs` (999 lines) — new
- `crates/cel-core/src/eval/conversions.rs` (223 lines) — new
- `crates/cel-core/src/eval/operators.rs` (402 lines) — new
- `crates/cel-core/src/eval/builtins.rs` (314 lines) — new
- `crates/cel-core/src/eval/resolve.rs` (277 lines) — new
- `crates/cel-core/src/eval/evaluator.rs` — slimmed from 3,505 to 1,349 lines
- `crates/cel-core/src/eval/mod.rs` — added 5 new module declarations

### Notable decisions
- All extracted code stays as `impl Evaluator<'a>` blocks — Rust allows multiple impl blocks across files in the same crate
- Tests remain in `evaluator.rs` since they exercise the full evaluator
- `eval_and` and `eval_or` stay in evaluator.rs (control flow/short-circuiting tied to core dispatch) but are `pub(super)` so `operators.rs` can delegate to them
- Struct fields on `Evaluator` are `pub(super)` so submodule files can access them

### Results
- 100% conformance maintained (4752/4752 tests passing, +0 vs baseline)
- All unit and integration tests passing
- evaluator.rs reduced by 62% (3,505 → 1,349 lines)

## Next Up
- GitHub Issue: #50 — Decouple prost/prost-reflect from cel-core via trait abstraction
  - Extract a trait-based interface so cel-core doesn't depend directly on prost/prost-reflect
  - Enables alternative protobuf backends and cleaner dependency boundaries
  - The new `proto.rs` submodule makes this refactoring easier since proto logic is now isolated
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
  - Related to #50; conversion logic is now cleanly separated in proto.rs

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — may revisit if additional test cases require it.
