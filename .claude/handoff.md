# Roadmap Handoff

## Last Updated
2026-01-31

## Just Completed
- [x] Legacy Enum Mode (Milestone 5.4 — `strong_enums` flag)
  - Added `strong_enums: bool` field to `Env` (default `true`), with `with_legacy_enums()` builder
  - Threaded through `Program::with_strong_enums()` → `Evaluator::with_strong_enums()`
  - Added `enum_or_int()` helper on `Evaluator` that returns `Value::Enum` or `Value::Int` based on flag
  - Applied at all 5 enum creation sites: reference map, proto field, proto scalar, enum constructor (int), enum constructor (string)
  - `CelConformanceService::with_strong_enums(bool)` constructor for test mode selection
  - Conformance tests: split `test_enums_eval` into `test_enums_legacy_eval` (legacy_proto2/proto3 sections, strong_enums=false) and `test_enums_strong_eval` (strong_proto2/proto3 sections, strong_enums=true) — both pass
  - Added `enums/` examples directory with `legacy.rs`, `strong.rs`, and `README.md`
  - Key files: `env.rs`, `eval/program.rs`, `eval/evaluator.rs`, `conformance/service.rs`, `conformance/tests/conformance.rs`

## Known Issues with This Change
- **Cross-type enum equality**: `EnumValue == Int` comparisons still return `false` (e.g., `GlobalEnum.GAR == 1`). Need to implement cross-type equality between `Enum` and `Int` values.
- **Enum arithmetic**: `TestAllTypes.NestedEnum.BAR + 3` fails because `_+_` has no overload for `Enum + Int`. Need to either auto-coerce enum to int in arithmetic or add overloads.
- **Enum in list membership**: `0 in [EnumValue]` and `EnumValue in [0]` fail due to cross-type comparison gap.

## Next Up: Cross-Type Enum Equality (5.4 remaining items)

### Why This Is Next
The strong enum implementation is nearly complete — legacy mode now works, but enum-to-int interop (equality, arithmetic, membership) is still missing. These are straightforward changes to the `Value` PartialEq implementation and operator dispatch.

### Tasks
1. **Cross-type equality**: Implement `Enum == Int` and `Int == Enum` in `PartialEq for Value`
2. **Enum arithmetic**: Auto-coerce `Enum` to `Int` in arithmetic operators (or add overloads)
3. **Enum membership**: Ensure `Int in [Enum]` and `Enum in [Int]` work via cross-type comparison

### Key Files
- `crates/cel-core/src/eval/value.rs` — `PartialEq` implementation, cross-type equality
- `crates/cel-core/src/eval/evaluator.rs` — operator dispatch, coercion

### Potential Challenges
- Determining when to coerce enum→int automatically vs requiring explicit `int()` conversion

## Alternate Next: Encoders Extension (5.1d)
If enum interop is deferred, the encoders extension (`base64.encode`/`base64.decode`) is a self-contained 4-failure fix.

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- Map serialization ordering is non-deterministic, causing flaky conformance test failures in `dynamic.textproto` and `proto3.textproto` for struct literal tests. Not a correctness issue but affects test stability.
- Remaining type_deduction improvements (parameterized type propagation through deep nesting, wrapper type promotion) are deferred — these would require deeper changes to the type inference engine.
