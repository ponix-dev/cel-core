# Roadmap Handoff

## Last Updated
2026-01-31

## Just Completed
- [x] Well-Known Type (WKT) Handling (Milestone 5.5)
  - Implemented `cel_value_to_google_value` support for `Bytes` (base64-encoded), `Duration` ("Xs" format), `Timestamp` (RFC 3339), `Proto(Empty)` (empty struct), and `Proto(FieldMask)` (comma-separated paths)
  - Added large integer overflow handling: Int/UInt values exceeding 2^53 serialize to `string_value` instead of lossy `number_value`
  - Hand-rolled `base64_encode` and `get_field_mask_paths` helper functions (no new dependencies)
  - Fixed proto converter `build_type_name_expr` to use struct expression IDs instead of 0 for synthetic type name nodes, enabling the checker to store resolved type references without collisions
  - Wrappers conformance: 12/36 → 36/36 (+24 tests)
  - Comparisons conformance: 391/406 → 401/406 (+10 tests, cross-type numeric comparisons now work for wrapper values)
  - Proto2 conformance: 101/108 → 106/108 (+5 tests)
  - Proto3 conformance: 73/75 → 75/75 (+2 tests)
  - Overall eval: 2199/2340 → 2238/2340 (+39 tests, 95.6% pass rate)
  - Key files: `crates/cel-core/src/eval/wkt.rs`, `crates/cel-core-proto/src/converter.rs`

## Known Issues
- **Dynamic regression**: `dynamic.textproto` eval went from 223/226 to 222/226 (-1). The `float/literal_not_double` test expects `google.protobuf.FloatValue{value: 1.333} == 1.333` to be `false` (float32 precision loss), but we return `true`. This is a pre-existing float precision issue that may have been exposed by the wrapper changes.
- **Map serialization ordering**: Non-deterministic map key ordering in `value_struct/field_assign_proto3` test (proto bytes differ only in key order).

## Next Up: Fix Flaky Conformance Tests (5.6)
### Why This Is Next
Non-deterministic map serialization causes conformance test results to vary between runs (±2-3 tests in `dynamic.textproto`, occasional jitter in `proto2.textproto` and `proto3.textproto`). This undermines the reliability of the conformance report and baseline comparison tooling.

### Root Cause
`google.protobuf.Struct` fields are serialized through `HashMap`, which has non-deterministic iteration order. When the resulting proto bytes are compared against expected values, different key ordering produces different bytes, causing spurious pass/fail.

Two call sites:
1. `crates/cel-core/src/eval/wkt.rs:430` — `cel_map_to_struct` builds `std::collections::HashMap` for Struct fields
2. `crates/cel-core/src/eval/evaluator.rs:2398` — map-to-proto conversion in `scalar_value_to_proto` uses `HashMap`

### Fix
Replace `HashMap::new()` with `BTreeMap::new()` at both sites. `prost_reflect::Value::Map` accepts any `HashMap<MapKey, Value>` — check whether `prost_reflect` supports `BTreeMap` directly, or sort the entries before insertion. The goal is deterministic proto byte serialization so that map-containing messages produce identical bytes across runs.

### Affected Tests
- `dynamic.textproto`: `struct/field_assign_proto2`, `struct/field_assign_proto3`, `value_struct/field_assign_proto2`, `value_struct/field_assign_proto3`
- `proto2.textproto`: `literal_wellknown/struct`
- Possibly others involving `google.protobuf.Struct` or `google.protobuf.Value` with map sub-fields

### Key Files
- `crates/cel-core/src/eval/wkt.rs` — `cel_map_to_struct`
- `crates/cel-core/src/eval/evaluator.rs` — map field assignment in `scalar_value_to_proto`

## Up Next: Encoders Extension (5.1d) — 4 failures
### Why This Is Next
Self-contained 4-failure fix. `base64.encode`/`base64.decode` checker declarations exist; just need evaluator runtime function registration.

### Tasks
1. Register `base64.encode` and `base64.decode` as evaluator runtime functions
2. Use the existing `base64_encode` helper in wkt.rs (or refactor to shared location)
3. Implement `base64.decode` (inverse of encode)

### Key Files
- `crates/cel-core/src/eval/evaluator.rs` — function dispatch
- `crates/cel-core/src/eval/wkt.rs` — existing `base64_encode` helper

## Alternate Next: Cross-Type Enum Equality (5.4 remaining)
Enum-to-int equality, arithmetic, and membership — ~5 failures in comparisons and enums.

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- Map serialization ordering is non-deterministic — see Milestone 5.6 above for fix plan.
- Remaining type_deduction improvements (parameterized type propagation through deep nesting, wrapper type promotion) are deferred — these would require deeper changes to the type inference engine.
