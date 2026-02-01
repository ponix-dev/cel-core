# Roadmap Handoff

## Last Updated
2026-02-01

## Just Completed
- GitHub Issue: #24 (5.4: Enum-to-int cross-type equality)
- Summary: Implemented cross-type equality and ordering between enum values and numeric types (int, uint, double)
- Key files: `crates/cel-core/src/eval/value.rs`, `crates/cel-core/src/eval/evaluator.rs`, `crates/cel-core-conformance/tests/conformance.rs`

### Changes
1. **Enum-numeric equality** — `Value::PartialEq` now handles `Enum == Int`, `Enum == UInt`, and `Enum == Double` comparisons (and their symmetric forms), treating enum values as their underlying integer
2. **Enum-numeric ordering** — `Value::compare` supports cross-type ordering between enums and all numeric types, enabling relational operators (`<`, `>`, `<=`, `>=`)
3. **WKT field access fix** — Narrowed the evaluator's "is WKT" check from a blanket `google.protobuf.*` prefix to `wkt::is_wrapper_type()` plus explicit `Any`, so non-wrapper types (Struct, Value, ListValue) are handled correctly
4. **Conformance test equivalence** — Added `EnumValue == Int64` matching in `values_equivalent` for conformance output comparison

### Results
- proto2.textproto: 106/108 → **108/108** (+2, now 100%)
- type_deduction.textproto: 19/22 → **22/22** (+3, now 100%)
- Overall: +5 eval tests, no regressions

## Next Up
- GitHub Issue: #25 — Any Type Support (5.6)
  - Would fix ~7 failures across comparisons.textproto and dynamic.textproto
  - Requires Any unpacking for equality comparison and empty Any literal validation
- GitHub Issue: #26 — Namespace & Qualified Identifier Resolution (5.7)
  - Would fix ~10 failures across namespace.textproto and fields.textproto
  - Dotted variable names like `a.b.c` need to resolve against container namespaces

## Alternate Next
- GitHub Issue: #29 — cel.block Extension (5.10)
  - Would fix 74 failures but is a larger feature
- GitHub Issue: #28 — Proto Extensions (5.9)
  - Would fix 36 failures, requires `proto.hasExt`/`proto.getExt`

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- Mixed-type map key equality (1 failure in comparisons.textproto) — map equality doesn't use cross-type numeric comparison for keys. This may need a dedicated fix.
