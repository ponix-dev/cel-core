# Roadmap Handoff

## Last Updated
2026-02-01

## Just Completed
- GitHub Issue: #25 (5.6: Any Type Support)
- [x] Any unpacking for equality comparison
- [x] Bytewise fallback comparison for unknown Any types
- [x] Empty Any literal validation (conversion error)
- [x] Map key numeric coercion for equality

### Summary
Added semantic equality for `google.protobuf.Any` messages and fixed map key comparison to use numeric coercion.

### Key files modified
- `crates/cel-core/src/eval/value.rs` — `any_semantic_eq` function, `ProtoValue::PartialEq`, map equality with `get_with_numeric_coercion`
- `crates/cel-core/src/eval/evaluator.rs` — Empty Any validation and auto-unwrap via `maybe_unwrap_well_known`

### Notable decisions
- Any comparison unpacks both messages to their inner type and compares semantically; falls back to bytewise comparison if the type can't be resolved in the descriptor pool
- Empty Any (no `type_url`) returns an `InvalidConversion` error rather than wrapping as a proto value
- `type_url` field access on Any is not yet implemented (not needed for the fixed tests)

### Results
- comparisons.textproto: 401/406 -> **406/406** (+5, now 100%)
- dynamic.textproto: 224/226 -> **226/226** (+2, now 100%)
- Overall: +7 eval tests, no regressions

## Next Up
- GitHub Issue: #26 — Namespace & Qualified Identifier Resolution (5.7)
  - Would fix ~10 failures across namespace.textproto and fields.textproto
  - Dotted variable names like `a.b.c` need to resolve against container namespaces
  - Requires changes to evaluator identifier resolution and activation lookup

## Alternate Next
- GitHub Issue: #29 — cel.block Extension (5.10)
  - Would fix 74 failures but is a larger feature
- GitHub Issue: #28 — Proto Extensions (5.9)
  - Would fix 36 failures, requires `proto.hasExt`/`proto.getExt`

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — this was listed in #25 but wasn't needed for the conformance test fixes. May revisit if additional test cases require it.
