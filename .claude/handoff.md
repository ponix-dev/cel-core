# Roadmap Handoff

## Last Updated
2026-01-28

## Just Completed
- [x] Error-as-value semantics (Milestone 5)
  - Non-boolean type errors in logical operators now use commutative evaluation
  - `'horses' && false` returns `false` (right side is definitive)
  - `'horses' || true` returns `true` (right side is definitive)
  - Type mismatches in logical operators now produce `no_matching_overload` errors (matching cel-go)
  - Logic conformance tests: 30/30 passing

### Summary
Completed error-as-value semantics for logical operators. The key change was treating non-boolean type errors the same as runtime errors for short-circuit purposes in `eval_and` and `eval_or`. When the left operand is a non-boolean value, the right operand is evaluated; if it can definitively determine the result (false for AND, true for OR), that result is returned instead of an error.

### Key Files Modified
- `crates/cel-core/src/eval/evaluator.rs` - Updated `eval_and()` and `eval_or()` to apply commutative evaluation for non-boolean type errors

### Notable Decisions
- Non-boolean types in logical operators produce `no_matching_overload` errors (not `type_mismatch`), matching cel-go behavior
- Commutative evaluation applies the same logic to type errors as to runtime `Value::Error` values

## Next Up
- [ ] Proto type conformance
  - `type(timestamp(...))` should return `google.protobuf.Timestamp`
  - Wrapper type comparison with proto types

## Open Questions
- Should `type(timestamp(...))` return `google.protobuf.Timestamp` or the CEL native `timestamp` type?
- How should optional field errors be handled in error-as-value semantics?
