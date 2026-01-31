# Roadmap Handoff

## Last Updated
2026-01-31

## Just Completed
- [x] Strong Enum Typing (Milestone 5.4)
  - Added `Value::Enum(EnumValue)` variant with fully-qualified type name and i32 value
  - Enum values from proto fields, checker references, and enum constructors now carry type info
  - `type()` on enum values returns the enum type name (e.g., `cel.expr.conformance.proto3.GlobalEnum`)
  - Enum constructor functions: `TestAllTypes.NestedEnum(1)` (int arg) and `GlobalEnum("BAZ")` (string arg)
  - `int()` conversion from enum values extracts the numeric value
  - Checker: `try_enum_constructor()` resolves enum constructor calls, stores `enum_type` in `ReferenceInfo`
  - Evaluator: `eval_enum_constructor()` handles int and string arguments with proto registry lookup
  - Conformance service: `value_to_proto_value` maps `Value::Enum` to proto `EnumValue`
  - Key files: `eval/value.rs`, `eval/evaluator.rs`, `checker/checker.rs`, `checker/overload.rs`, `conformance/service.rs`
- Net conformance improvement: +18 parse+check (enums), +8 eval (enums), +1 eval (proto3), total +57 passed/+60 total

## Known Issues with This Change
- **Legacy enum tests fail (25 failures)**: The `legacy_proto2` and `legacy_proto3` enum test sections expect `Int64Value` results, but we always return `EnumValue`. The conformance spec has a `strong_enums` flag that should control this behavior — when false, enums should behave as plain ints.
- **Cross-type enum equality**: `EnumValue == Int` comparisons return `false` (e.g., `GlobalEnum.GAR == 1`). Need to implement cross-type equality between `Enum` and `Int` values.
- **Enum arithmetic**: `TestAllTypes.NestedEnum.BAR + 3` fails because `_+_` has no overload for `Enum + Int`. Need to either auto-coerce enum to int in arithmetic or add overloads.
- **Enum in list membership**: `0 in [EnumValue]` and `EnumValue in [0]` fail due to cross-type comparison gap.
- **type_deduction regression (-1)**: `TestAllTypes{}.standalone_enum` returns `EnumValue` instead of `Int64Value` for a test that expects legacy behavior.

## Next Up: Finish Enum Interop (5.4 remaining items)

### Why This Is Next
The strong enum implementation is partially complete — the representation and constructors work, but backward-compatible interop with legacy (weak) enum mode is missing. This causes 25+ conformance failures that should be easy to fix.

### Tasks
1. **Cross-type equality**: Implement `Enum == Int` and `Int == Enum` in `PartialEq for Value`
2. **Enum arithmetic**: Auto-coerce `Enum` to `Int` in arithmetic operators (or add overloads)
3. **Legacy enum mode**: Use the `strong_enums` flag from conformance tests to return `Int64Value` when strong enums are disabled
4. **Enum membership**: Ensure `Int in [Enum]` and `Enum in [Int]` work via cross-type comparison

### Key Files
- `crates/cel-core/src/eval/value.rs` — `PartialEq` implementation, cross-type equality
- `crates/cel-core/src/eval/evaluator.rs` — operator dispatch, coercion
- `crates/cel-core-conformance/src/service.rs` — `strong_enums` flag handling
- `crates/cel-core-conformance/src/lib.rs` — conformance service trait

### Potential Challenges
- Determining when to coerce enum→int automatically vs requiring explicit `int()` conversion
- The `strong_enums` flag may need threading through the `Env`/`Program` configuration

## Alternate Next: Encoders Extension (5.1d)
If enum interop is deferred, the encoders extension (`base64.encode`/`base64.decode`) is a self-contained 4-failure fix.

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- Map serialization ordering is non-deterministic, causing flaky conformance test failures in `dynamic.textproto` and `proto3.textproto` for struct literal tests. Not a correctness issue but affects test stability.
- Remaining type_deduction improvements (parameterized type propagation through deep nesting, wrapper type promotion) are deferred — these would require deeper changes to the type inference engine.
