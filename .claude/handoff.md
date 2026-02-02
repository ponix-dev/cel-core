# Roadmap Handoff

## Last Updated
2026-02-02

## Just Completed
- GitHub Issue: #26 (5.7: Namespace & Qualified Identifier Resolution)
- [x] Qualified variable identifiers (`a.b.c` where `a.b` is the variable name)
- [x] Container namespace shadowing and lookup

### Summary
Added container-aware identifier resolution to both the type checker and evaluator. Identifiers are now resolved using C++ namespace-style rules: container-prefixed names (most qualified to least) are tried first, then the bare name. Local variables (comprehension, bind) properly shadow container-prefixed names. The evaluator now supports longest-prefix matching for qualified variable names and correctly handles leading-dot (RootIdent) resolution via a preserved root activation.

### Key files modified
- `crates/cel-core/src/checker/checker.rs` — `check_ident` container-prefixed resolution, `check_member` skips qualified resolution when leftmost ident is local, `leftmost_ident_resolves` helper
- `crates/cel-core/src/checker/scope.rs` — `is_local` method to distinguish local vs root scope variables
- `crates/cel-core/src/eval/evaluator.rs` — `resolve_with_container`, `eval_root_ident`, `try_qualified_variable_name`, `leftmost_ident_resolves`, `try_longest_prefix_match`, `root_activation` tracking, `in_local_scope` flag

### Notable decisions
- Local variables (comprehension, bind) always shadow container-prefixed names — this matches cel-go behavior where `x` in `[1,2,3].map(x, x)` should never resolve as `container.x`
- Leading-dot identifiers (`.x`) resolve against the root activation only, bypassing container prefixing and local scope entirely
- The evaluator uses longest-prefix matching: for `a.b.c`, it tries `a.b.c` as a variable, then `a.b` + field `.c`, then `a` + fields `.b.c`
- The checker's `check_member` skips qualified name resolution when the leftmost identifier resolves in scope, preventing comprehension variables from being misinterpreted as namespace prefixes

### Results
- namespace.textproto: 8/14 -> **14/14** (+6, now 100%)
- fields.textproto: 56/60 -> **60/60** (+4, now 100%)
- Overall: +10 eval tests, no regressions

## Next Up
- GitHub Issue: #28 — Proto Extensions (5.9)
  - Would fix 36 failures in proto2_ext.textproto
  - Requires implementing `proto.hasExt` and `proto.getExt` functions
  - Needs proto2 extension field descriptor resolution
- GitHub Issue: #29 — cel.block Extension (5.10)
  - Would fix 74 failures in block_ext.textproto (largest single block)
  - Requires `cel.block` and `cel.index` function implementations

## Open Questions
- `google.protobuf.Timestamp` and `google.protobuf.Duration` as type identifiers still fail (2 eval tests in timestamps.textproto) — these qualified proto type names need to be resolvable as values in the evaluator. Could be addressed as part of #26 remaining scope or a separate issue.
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — may revisit if additional test cases require it.
