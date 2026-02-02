# Roadmap Handoff

## Last Updated
2026-02-02

## Just Completed
- GitHub Issue: #28 (5.9: Proto Extensions)
- [x] `proto.hasExt(msg, ext)` macro — expands to `has(msg.ext)` presence test
- [x] `proto.getExt(msg, ext)` macro — expands to `msg.ext` select expression
- [x] Qualified identifier validation for extension field arguments

### Summary
Implemented `proto.hasExt` and `proto.getExt` as standard macros in the parser. These macros handle proto2 extension field access by expanding at parse time: `proto.getExt(msg, pkg.ExtField)` becomes a member select `msg.pkg.ExtField`, and `proto.hasExt(msg, pkg.ExtField)` becomes a presence test `has(msg.pkg.ExtField)`. Both validate that the second argument is a qualified identifier (ident or dotted member chain).

### Key files modified
- `crates/cel-core/src/parser/macros.rs` — Added `proto.hasExt` and `proto.getExt` macro definitions, `validate_qualified_identifier` helper, expansion functions, and unit tests

### Notable decisions
- Implemented as macros (not runtime functions) matching the CEL spec approach where proto extensions are resolved at parse/check time
- The second argument is validated as a qualified identifier at macro expansion time, rejecting non-identifier expressions (e.g., literals, calls)
- `proto.getExt` expands to `Expr::Member` and `proto.hasExt` expands to `Expr::MemberTestOnly`, reusing existing AST nodes

### Results
- proto2_ext.textproto: 0/18 -> **18/18** (+18, now 100%)
- Overall: +36 conformance tests (18 parse+check, 18 eval), no regressions

## Next Up
- GitHub Issue: #29 — cel.block Extension (5.10)
  - Would fix 74 failures in block_ext.textproto (largest single remaining block)
  - Requires `cel.block` and `cel.index` function implementations
  - `cel.block` takes a list of bindings and an expression body, `cel.index(N)` references the Nth binding
- GitHub Issue: #30 — Miscellaneous Behavioral Fixes (5.11)
  - Various small fixes and edge cases

## Open Questions
- `google.protobuf.Timestamp` and `google.protobuf.Duration` as type identifiers still fail (2 eval tests in timestamps.textproto) — these qualified proto type names need to be resolvable as values in the evaluator.
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — may revisit if additional test cases require it.
