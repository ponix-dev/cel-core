# Roadmap Handoff

## Last Updated
2026-02-03

## Just Completed
- Refactored trait abstraction naming for clarity:
  - Split `TypeRegistry` into two focused traits: `ProtoTypeResolver` (checker) and `ProtoRegistry` (evaluator)
  - Renamed `ProstTypeRegistry` → `ProstProtoRegistry`
  - Renamed `type_registry` field/method → `proto_registry`
  - Renamed `with_type_registry()` → `with_proto_registry()`
  - Renamed `check_with_type_registry()` → `check_with_descriptor_pool()`

### Summary
Split the monolithic `TypeRegistry` trait into two focused traits:
- `ProtoTypeResolver` — checker methods for type lookup (get_field_type, get_enum_value, resolve_qualified, resolve_message_name)
- `ProtoRegistry` — evaluator methods (construct_message, message_field_access, etc.) + inherits ProtoTypeResolver

This better separates concerns: the checker only needs type resolution, while the evaluator needs runtime operations.

### Key files modified
- `crates/cel-core/src/eval/proto_registry.rs` — defines `ProtoTypeResolver` and `ProtoRegistry` traits (renamed from type_registry.rs)
- `crates/cel-core/src/eval/mod.rs` — exports `ProtoTypeResolver`, `ProtoRegistry`, `StructFieldValue`
- `crates/cel-core/src/checker/checker.rs` — uses `&dyn ProtoTypeResolver`
- `crates/cel-core/src/env.rs` — uses `Arc<dyn ProtoRegistry>`, renamed to `proto_registry`
- `crates/cel-core/src/eval/evaluator.rs` — uses `&dyn ProtoRegistry`
- `crates/cel-core-proto/src/registry.rs` — `ProstProtoRegistry` implements both traits
- `crates/cel-core-proto/src/eval_proto.rs` — `impl ProtoRegistry for ProstProtoRegistry`

## Previous Work
- GitHub Issue: #50 — Decouple prost/prost-reflect from cel-core via trait abstraction
- Defined `MessageValue` trait in `cel-core::eval::message` — runtime proto message abstraction
- Moved `ProtoValue` to `cel-core-proto` as `ProstMessage` implementing `MessageValue`
- Moved WKT handling (`wkt.rs`) and proto evaluation logic from cel-core to cel-core-proto
- Updated `Value::Proto(ProtoValue)` → `Value::Message(Box<dyn MessageValue>)`
- Removed `prost` and `prost-reflect` from cel-core's Cargo.toml

## Next Up
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
  - `proto_value_to_value()` and `value_to_proto_value()` in conformance service.rs should become public API in cel-core-proto
  - `bindings_to_activation()` and `convert_function_decl()` should also move
  - This completes the proto interop story for external users

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — may revisit if additional test cases require it.
