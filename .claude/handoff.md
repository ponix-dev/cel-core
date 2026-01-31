# Roadmap Handoff

## Last Updated
2026-01-31

## Just Completed
- [x] Two-variable macro forms (Milestone 5.3)
  - All two-variable comprehension macros now work: `all(i, v, ...)`, `exists(i, v, ...)`, `exists_one(i, v, ...)`, `transformList(i, v, ...)`, `transformMap(k, v, ...)`
  - Added `existsOne` camelCase alias for cel-go compatibility
  - Fixed evaluator iter_var/iter_var2 binding order (iter_var=index, iter_var2=element for lists)
  - Fixed checker to infer correct types for two-variable form (index=Int, value=element type)
- [x] Map `+` operator (Milestone 5.2, partial)
  - Implemented map merging via `+` operator in evaluator, needed by `transformMap` macro
  - Added `add_map_map` overload to standard library for type checking
- [x] Comprehension exhaustive evaluation (Milestone 5.11, partial)
  - Loop condition errors no longer short-circuit (CEL "not strictly false" semantics)
  - Loop step errors no longer return immediately; error propagates through accumulator
  - `exists_one` now iterates all elements (loop condition changed from `accu <= 1` to `true`)
- Key files: `evaluator.rs`, `macros.rs`, `checker.rs`, `standard_library.rs`
- Net conformance improvement: +2 parse+check, +4 eval (macros 42→44, macros2 44→46 parse+check, 45→46 eval)

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
