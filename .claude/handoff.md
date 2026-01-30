# Roadmap Handoff

## Last Updated
2026-01-30

## Just Completed
- Proto message type conformance improvements (Milestone 5)
  - Extracted `wkt.rs` module for well-known type unwrapping (shared between evaluator and conformance service)
  - Fixed conformance service to pass container and type declarations through to checker/evaluator for proper name resolution
  - Added WKT field presence handling in evaluator (unset wrapper fields return null, unset ListValue/Struct return empty defaults)
  - Added reference map constant value resolution for enum values in evaluator
  - Fixed map `in` operator numeric coercion (double keys coerced to int/uint for lookup)
  - Fixed triple-quoted string escape sequence processing in lexer
  - Added detailed Milestone 5 breakdown with categorized remaining conformance failures (~601 remaining)
  - Key files: `crates/cel-core/src/eval/wkt.rs` (new), `crates/cel-core/src/eval/evaluator.rs`, `crates/cel-core-conformance/src/service.rs`, `crates/cel-core/src/parser/lexer.rs`
  - Conformance score: 1469/2070 individual eval tests passing (71.0%), up from ~1091 (49.5%)

## Next Up: String Extension Functions (5.1a)

### Why This Is Next
String extension functions account for 163 conformance failures — the second-largest category after math extensions. The checker already declares these functions; only the evaluator runtime implementations are missing.

### Functions to Implement
- `charAt`, `indexOf`, `lastIndexOf` — string indexing
- `lowerAscii`, `upperAscii` — ASCII case conversion
- `replace`, `split`, `substring`, `trim` — string manipulation
- `join` — list-to-string joining
- `reverse` — string reversal
- `format` — printf-style string formatting
- `strings.quote` — string quoting

### Key Files
- `crates/cel-core/src/eval/evaluator.rs` — `eval_call()` dispatches function calls
- `crates/cel-core/src/eval/mod.rs` — module exports
- Conformance test file: `string_ext.textproto`

### Potential Challenges
- `format` is complex (printf-style with `%s`, `%d`, `%f`, `%e`, `%x`, `%o`, `%b`)
- Unicode handling for `charAt`, `indexOf`, `reverse` (CEL operates on code points)

## Open Questions
- Should extension function implementations live in a separate module (e.g., `eval/ext/strings.rs`) or inline in evaluator?
- The `format` function may need its own parser for format strings
