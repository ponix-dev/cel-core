# Roadmap Handoff

## Last Updated
2026-02-02

## Just Completed
- GitHub Issue: #30 (5.11: Miscellaneous Behavioral Fixes) — partial
- [x] Qualified proto message type resolution in evaluator

### Summary
Added proto message type resolution to the evaluator's `resolve_qualified_identifier` function. When a qualified identifier (e.g., `google.protobuf.Timestamp`) is not found in the activation, the evaluator now checks if it matches a known proto message type in the registry and returns it as a `Value::Type`. This fixes 2 eval failures in timestamps.textproto.

### Key files modified
- `crates/cel-core/src/eval/evaluator.rs` — Added proto type registry lookup as fallback in `resolve_qualified_identifier`
- `.claude/context/conformance-baseline.md` — Updated baseline (timestamps.textproto eval 74→76)

### Notable decisions
- The proto type lookup is a fallback after activation lookup, so user-defined variables still take precedence over proto type names
- Uses the existing `proto_types` registry and `resolve_message_name` with container support

### Results
- timestamps.textproto eval: 74/76 → **76/76** (+2, now 100%)
- Overall: +2 conformance tests, no regressions

## Next Up
- GitHub Issue: #30 — Miscellaneous Behavioral Fixes (5.11) — remaining tasks
  - `dyn()` type equality: different message types should not be equal
  - FloatValue precision: float32 vs float64 precision loss detection
  - `google.protobuf.Any` literal construction validation
  - `has()` on explicitly-set message fields returns `true`
  - `has()` on optional map entries with `optional.none()` values
  - Proto map key serialization ordering (deterministic output)
- GitHub Issue: #29 — cel.block Extension (5.10)
  - Would fix 74 failures in block_ext.textproto (largest single remaining block)

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — may revisit if additional test cases require it.
