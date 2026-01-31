# Roadmap Handoff

## Last Updated
2026-01-31

## Just Completed
- [x] Optional extension function implementations (Milestone 5.1c)
  - Added `.with_impl(...)` runtime implementations to all optional extension function overloads in `optionals_ext.rs`
  - All 70/70 optionals conformance eval tests now pass (up from 10/70)
  - Key functions: `optional.of`, `optional.none`, `optional.ofNonZeroValue` (constructors), `hasValue`, `value`, `or`, `orValue` (methods)
  - Implemented `is_zero_value()` helper for `ofNonZeroValue` — checks null, false, 0, 0.0, empty strings/bytes/lists/maps, and default proto messages
  - Added optional chaining in evaluator: `x.?y` (field), `x[?key]` (index) — unwraps optional values, returns `optional.none()` on missing
  - Added `has()` support for optional values — checks field presence on wrapped maps/protos
  - Added `optional_type` to `TypeValue` for `type(optional.none())` expressions
  - Handled unset repeated/map proto fields returning `optional.none()` in optional field access
  - Key files: `crates/cel-core/src/ext/optionals_ext.rs`, `crates/cel-core/src/eval/evaluator.rs`, `crates/cel-core/src/eval/value.rs`
  - Net conformance improvement: +59 eval tests (1753/1812, 96.7%)

## Next Up: Encoders Extension Functions (5.1d)

### Why This Is Next
Encoders extension functions account for 4 conformance failures — a small but complete extension library. All other extension categories (5.1a-c) are now done, making this the final extension to implement.

### Functions to Implement
- `base64.encode` — encode bytes to base64 string
- `base64.decode` — decode base64 string to bytes

### Key Files
- `crates/cel-core/src/ext/encoders_ext.rs` — encoder extension declarations (needs `.with_impl(...)`)
- Conformance test file: `encoders_ext.textproto`

### Potential Challenges
- Need to determine which base64 variant to use (standard vs URL-safe, padding vs no-padding)
- May need to add a `base64` crate dependency

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- Map serialization ordering is non-deterministic, causing flaky conformance test failures in `dynamic.textproto` and `proto3.textproto` for struct literal tests. Not a correctness issue but affects test stability.
