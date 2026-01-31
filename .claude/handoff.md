# Roadmap Handoff

## Last Updated
2026-01-31

## Just Completed
- [x] Type checker inference improvements (Milestone 5.8, partial)
  - Scoped type parameter resolution: each overload match now gets unique scope IDs for type params, preventing cross-expression collision where `T` from one call would bleed into another
  - Type parameter binding widening: when a type param is bound to `Null` or `Dyn` and a more specific type appears, the binding is widened to the concrete type
  - Null assignability expanded: `Null` is now assignable to `Message`, `Timestamp`, `Duration`, `Optional`, and `Abstract` types (not just `Wrapper`)
  - Custom function type declarations: conformance tests can now declare custom functions (`fn`, `tuple`, `sort`) via `FunctionTypeDecl` passed through the conformance service
  - Comprehension accumulator type refinement: when the accumulator starts with an unresolved type (e.g., empty list from `map` macro), the loop step type refines it
  - Type specificity-based `join_types`: instead of picking the first type, picks the most specific type that all others are assignable to
  - Abstract type matching in overload resolution
  - `optional_type` proto name fix for Optional type conversion (matching cel-go wire format)
  - Key files: `checker.rs`, `overload.rs`, `types/mod.rs`, `type_conversion.rs`, `conformance/service.rs`, `conformance/tests/conformance.rs`
  - Net conformance improvement: +5 parse+check, +22 type_check, +1 eval (type_deduction.textproto now fully passing)

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
- Remaining type_deduction improvements (parameterized type propagation through deep nesting, wrapper type promotion) are deferred — these would require deeper changes to the type inference engine.
