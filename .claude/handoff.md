# Roadmap Handoff

## Last Updated
2026-01-30

## Just Completed
- [x] String extension function implementations (Milestone 5.1a)
  - Implemented all string extension functions with runtime (`with_impl`) support: `charAt`, `indexOf`, `lastIndexOf`, `lowerAscii`, `upperAscii`, `replace`, `split`, `substring`, `trim`, `join`, `reverse`, `format`, `strings.quote`
  - Added namespaced function dispatch in the evaluator — mirrors the checker's `try_qualified_function_name` logic so `strings.quote(...)`, `math.greatest(...)`, etc. resolve correctly at eval time
  - All 216/216 string_ext conformance eval tests now pass (was 53/216)
  - Key files: `crates/cel-core/src/ext/string_ext.rs` (function implementations), `crates/cel-core/src/eval/evaluator.rs` (namespace dispatch)
  - The `format` function supports `%s`, `%d`, `%f`, `%e`, `%x`, `%o`, `%b` verbs with width/precision, plus locale-aware list/map formatting
  - Unicode-correct code point handling throughout (charAt, indexOf, substring, reverse all operate on code points, not bytes)

## Next Up: Math Extension Functions (5.1b)

### Why This Is Next
Math extension functions account for 182 conformance failures — the largest remaining category. The checker already declares all math functions; only the evaluator runtime implementations (`.with_impl(...)`) are missing. The namespaced function dispatch fix from this PR means `math.greatest(...)` etc. will work once implementations are added.

### Functions to Implement
- `math.greatest`, `math.least` — min/max across numeric types (variadic, 1-6 args + list)
- `math.ceil`, `math.floor`, `math.round`, `math.trunc` — rounding (double → double)
- `math.abs`, `math.sign` — absolute value and sign (int/uint/double)
- `math.isNaN`, `math.isInf`, `math.isFinite` — float classification
- `math.bitAnd`, `math.bitOr`, `math.bitXor`, `math.bitNot` — bitwise ops
- `math.bitShiftLeft`, `math.bitShiftRight` — bit shifting

### Key Files
- `crates/cel-core/src/ext/math_ext.rs` — add `.with_impl(...)` to each `OverloadDecl`
- Conformance test file: `math_ext.textproto`

### Potential Challenges
- `math.greatest`/`math.least` have many overloads (unary, binary same-type, binary mixed-type, ternary through 6-ary, list variants) — each needs its own implementation
- Mixed-type comparisons (int vs uint vs double) need careful handling to match CEL spec behavior
- Bitwise operations on uint need to handle the full u64 range

## Open Questions
- The string `format` function was implemented inline in `string_ext.rs` (~300 lines). Consider whether math extension implementations should follow the same pattern or use a helper module.
- Conformance score is now ~1632/2070 individual eval tests passing (~78.8%), up from ~1469 (71.0%)
