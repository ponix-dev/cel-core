# Roadmap Handoff

## Last Updated
2026-01-30

## Just Completed
- [x] Math extension function implementations (Milestone 5.1b)
  - Added `.with_impl(...)` runtime implementations to all math extension function overloads in `math_ext.rs`
  - All 199/199 math_ext conformance eval tests now pass
  - Key functions: `ceil`, `floor`, `round` (half away from zero), `trunc`, `abs` (with i64::MIN overflow check), `sign`, `isNaN`, `isInf`, `isFinite`, bitwise ops, bit shifts, `greatest`/`least`
  - `greatest`/`least` use shared helper functions with cross-type numeric comparison (int/uint/double), NaN handling, list support, and empty-list errors
  - Bit shifts use logical (unsigned) right shift per CEL spec; large shifts (>= 64) return 0; negative shifts return error
  - Added fallback `(UInt, Int)` handling in `int_int` shift overloads to work around overload resolution selecting the wrong overload for uint operands
  - Key file: `crates/cel-core/src/ext/math_ext.rs`
  - Conformance score: 2083/2340 eval tests passing (89.0%)

## Next Up: Optional Extension Functions (5.1c)

### Why This Is Next
Optional extension functions account for 60 conformance failures — the next largest extension category. The checker already declares optional types and functions; only the evaluator runtime implementations are missing.

### Functions to Implement
- `optional.of`, `optional.none`, `optional.ofNonZeroValue` — constructors via namespace
- `hasValue`, `value`, `orValue` — optional value access methods
- Optional chaining support (`x.?y`, `x[?key]`) in evaluator

### Key Files
- `crates/cel-core/src/ext/` — optional extension declarations
- Conformance test file: `optionals.textproto`

### Potential Challenges
- Optional chaining (`x.?y`, `x[?key]`) requires evaluator changes beyond just function implementations
- Need to handle the interaction between optionals and other types (e.g., optional field access on proto messages)

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
