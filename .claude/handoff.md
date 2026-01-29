# Roadmap Handoff

## Last Updated
2026-01-29

## Just Completed
- [x] Error-as-value semantics (Milestone 5)
  - Non-boolean type errors in logical operators now use commutative evaluation
  - Logic conformance tests: 30/30 passing
  - Branch: `error-as-value-semantics`

## Next Up: Proto Message Type Conformance

### The Problem

388 conformance test failures mention "unknown message type" — the evaluator has a `ProtoTypeRegistry` but can't resolve short names like `TestAllTypes` to their fully-qualified forms. This is the single largest source of conformance failures, affecting 279 failures across 6 test files:

| File | Passed | Failed | Total |
|------|--------|--------|-------|
| proto2.textproto | 18 | 90 | 108 |
| proto3.textproto | 19 | 56 | 75 |
| enums.textproto | 16 | 69 | 85 |
| fields.textproto | 50 | 10 | 60 |
| wrappers.textproto | 0 | 36 | 36 |
| proto2_ext.textproto | 0 | 18 | 18 |

### Root Cause Analysis

The conformance tests set a `container` field (e.g., `cel.expr.conformance.proto2`) which should allow `TestAllTypes` to resolve to `cel.expr.conformance.proto2.TestAllTypes`. The resolution chain in the evaluator is:

1. **Reference map** (from type checker) — highest priority
2. **Abbreviations** (namespace shortcuts)
3. **Container-based resolution** (`ProtoTypeRegistry::resolve_message_name()`)

The issue is that the conformance service passes the container to the environment, but the type checker and/or evaluator isn't properly using it to resolve message type names in struct expressions.

### Key Files

| File | Role |
|------|------|
| `crates/cel-core-conformance/src/service.rs` | Creates `ProtoTypeRegistry`, adds descriptors, passes to `Env` |
| `crates/cel-core/src/eval/evaluator.rs` | `eval_struct()` (line ~1608) — constructs messages, `resolve_type_name()` (line ~1685) |
| `crates/cel-core/src/types/proto.rs` | `ProtoTypeRegistry` — wraps `prost_reflect::DescriptorPool`, has `resolve_message_name()` |
| `crates/cel-core/src/checker/checker.rs` | Pre-resolves type names, stores in `reference_map` |
| `crates/cel-core/src/env.rs` | Stores registry, passes container/registry to checker and evaluator |

### How `eval_struct()` Works (evaluator.rs ~1608-1683)

1. Calls `resolve_type_name()` to get fully-qualified name
2. Calls `registry.get_message(&fq_name)` to get `MessageDescriptor`
3. If not found: `"unknown message type: {name} (registry has proto_types: true)"`
4. Creates `DynamicMessage`, sets fields, unwraps well-known types

### How Conformance Tests Provide Type Info

Test files (`.textproto`) specify:
- `container: "cel.expr.conformance.proto2"` — namespace for resolution
- `expr: "TestAllTypes{single_int32: -34}"` — uses short name
- Expected result uses FQ name: `[type.googleapis.com/cel.expr.conformance.proto2.TestAllTypes]`

The conformance service (`service.rs`) registers all needed descriptors:
- `cel.expr` descriptors
- `cel.expr.conformance.proto2` test types
- `cel.expr.conformance.proto3` test types

### Secondary Proto Failures (not "unknown message type")

Even after fixing resolution, additional failures will remain:
1. **`has()` on constructed messages** — `has(TestAllTypes{single_int32: 16}.single_int32)` returns `false` instead of `true` (~24 failures in proto2)
2. **Proto2 extension fields** — accessing extension fields via backtick-quoted FQ names (`msg.\`cel.expr.conformance.proto2.int32_ext\``) returns "field not found"

### Conformance Score
- Current: 1091/2203 individual test cases passing (49.5%)
- Fixing proto message resolution could recover up to ~279 additional test cases

## Open Questions
- Is the container not being passed through to the evaluator's `resolve_type_name()`?
- Does the type checker need container awareness to populate the reference map for struct type names?
- Should `resolve_message_name()` be called earlier (at check time) so the reference map has FQ names?
