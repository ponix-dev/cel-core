# Roadmap Handoff

## Last Updated
2026-02-01

## Just Completed
- GitHub Issue: #22 (Encoders Extension)
- [x] Fix Flaky Conformance Tests (Milestone 5.6 handoff item)
  - Root cause: `ObjectValue` comparison in `conformance.rs` used byte-level `a.value == e.value`, which fails when `HashMap` iteration order differs between runs for proto messages containing map fields (e.g., `google.protobuf.Struct`)
  - Fix: Structural comparison via `DynamicMessage` decoding — both `Any` payloads are decoded using the shared descriptor pool, then compared with `PartialEq` (which handles map ordering correctly)
  - Extracted `build_descriptor_pool()` from `loader.rs` for reuse in test comparisons
  - `dynamic.textproto` eval results now stable at 224/226 across multiple runs (was ±2-3 jitter)
  - Key files: `crates/cel-core-conformance/src/loader.rs`, `crates/cel-core-conformance/tests/conformance.rs`

- [x] Encoders Extension Runtime (Milestone 5.1d)
  - Added `.with_impl()` to `base64.encode` and `base64.decode` overload declarations
  - Made `base64_encode` in `wkt.rs` `pub(crate)` for reuse
  - Implemented `base64_decode` with RFC 4648 support (padded and unpadded input)
  - Encoders conformance: 0/4 → 4/4 (all passing)
  - Key files: `crates/cel-core/src/ext/encoders_ext.rs`, `crates/cel-core/src/eval/wkt.rs`

## Known Issues
- **Dynamic regression**: `dynamic.textproto` eval at 224/226. The `float/literal_not_double` test expects `google.protobuf.FloatValue{value: 1.333} == 1.333` to be `false` (float32 precision loss), but we return `true`. Pre-existing float precision issue.
- 12 conformance test suites still failing (pre-existing): block_ext, comparisons, conversions, dynamic, fields, namespace, proto2, proto2_ext, timestamps, type_deduction

## Next Up: Cross-Type Enum Equality (5.4 remaining)
- GitHub Issue: #24
- Enum-to-int equality (`EnumValue == Int` and `Int in [EnumValue]`) — remaining item in strong enum typing milestone.

## Alternate Next: Conversion & Operator Edge Cases (5.2)
- GitHub Issue: #23
- `bool()` from string: accept `"1"`, `"0"`, `"t"`, `"f"`, `"TRUE"`, etc.
- `int()` / `uint()` from out-of-range doubles: error instead of silent clamping
- Repeated map key detection in map literals

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- Remaining type_deduction improvements (parameterized type propagation through deep nesting, wrapper type promotion) are deferred — these would require deeper changes to the type inference engine.
