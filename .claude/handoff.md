# Roadmap Handoff

## Last Updated
2026-02-01

## Just Completed
- GitHub Issue: #23 (5.2: Conversion & Operator Edge Cases)
- Summary: Fixed three categories of conformance failures in the evaluator
- Key file: `crates/cel-core/src/eval/evaluator.rs`

### Changes
1. **`bool()` string conversion** — Accept `"TRUE"`, `"True"`, `"FALSE"`, `"False"`, `"t"`, `"f"`, `"1"`, `"0"` in addition to `"true"`/`"false"`
2. **`int()`/`uint()` from out-of-range doubles** — Return overflow errors for NaN, infinity, and values outside the representable range instead of silently clamping
3. **Repeated map key detection** — Map literal construction now checks for duplicate keys (with numeric coercion) and returns an error for `{1: "a", 1: "b"}` style expressions

### Results
- conversions.textproto: 96/109 → **109/109** (+13, now 100%)
- fields.textproto: 54/60 → **56/60** (+2)
- Overall: +15 eval tests, no regressions

## Next Up
- GitHub Issue: #24 — Enum-to-int cross-type equality (5.4)
- `EnumValue == Int` and `Int in [EnumValue]` comparisons
- 1 known conformance failure in type_deduction.textproto (`standalone_enum` returns EnumValue instead of Int)
- Logical next step: small, focused change in the evaluator's equality/comparison logic

## Alternate Next
- GitHub Issue: #25 — Any Type Support (5.6)
- GitHub Issue: #26 — Namespace & Qualified Identifier Resolution (5.7)
  - Would fix ~10 failures across namespace.textproto and fields.textproto

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- Remaining type_deduction improvements (parameterized type propagation through deep nesting, wrapper type promotion) are deferred — these would require deeper changes to the type inference engine.
