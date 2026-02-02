# Roadmap Handoff

## Last Updated
2026-02-02

## Just Completed
- GitHub Issue: #50 — Decouple prost/prost-reflect from cel-core via trait abstraction
- [x] Define `MessageValue` trait in `cel-core::eval::message` — runtime proto message abstraction
- [x] Define `TypeRegistry` trait in `cel-core::eval::type_registry` — type resolution and message construction
- [x] Move `ProtoValue` to `cel-core-proto` as `ProstMessage` implementing `MessageValue`
- [x] Move `ProtoTypeRegistry` to `cel-core-proto` as `ProstTypeRegistry` implementing `TypeRegistry`
- [x] Move WKT handling (`wkt.rs`) and proto evaluation logic (`proto.rs`) from cel-core to cel-core-proto
- [x] Update `Value::Proto(ProtoValue)` → `Value::Message(Box<dyn MessageValue>)`
- [x] Update evaluator, checker, program, and env to use trait-based APIs
- [x] Remove `prost` and `prost-reflect` from cel-core's Cargo.toml

### Summary
Decoupled cel-core from prost/prost-reflect by introducing two trait abstractions: `MessageValue` (for runtime proto message values) and `TypeRegistry` (for type resolution, field access, message construction, and WKT handling). The prost-backed implementations now live in cel-core-proto. This removes ~1,700 lines of prost-specific code from cel-core (proto.rs and wkt.rs deleted entirely) and eliminates the prost/prost-reflect dependency from the core crate.

### Key files added/modified
- `crates/cel-core/src/eval/message.rs` — new `MessageValue` trait
- `crates/cel-core/src/eval/type_registry.rs` — new `TypeRegistry` trait with `StructFieldValue`
- `crates/cel-core-proto/src/message.rs` — `ProstMessage` implementing `MessageValue`
- `crates/cel-core-proto/src/registry.rs` — `ProstTypeRegistry` implementing `TypeRegistry`
- `crates/cel-core-proto/src/eval_proto.rs` — proto-to-CEL value conversion (moved from eval/proto.rs)
- `crates/cel-core-proto/src/wkt.rs` — well-known type handling (moved from eval/wkt.rs)
- `crates/cel-core/src/eval/value.rs` — `Value::Proto` → `Value::Message`
- `crates/cel-core/src/eval/evaluator.rs` — uses trait methods instead of direct prost calls
- `crates/cel-core/Cargo.toml` — removed prost, prost-reflect dependencies

### Notable decisions
- Used `Box<dyn MessageValue>` for type-erased message storage in `Value` enum rather than generics to avoid propagating type parameters throughout the evaluator
- `MessageValue` includes `as_any()` for downcasting and `clone_boxed()` for clonability
- `TypeRegistry` uses `&dyn` trait objects passed through the evaluator, keeping the public API simple
- WKT handling (Any unpacking, Timestamp/Duration construction) moved entirely into the `TypeRegistry` implementation

## Next Up
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
  - `proto_value_to_value()` and `value_to_proto_value()` in conformance service.rs should become public API in cel-core-proto
  - `bindings_to_activation()` and `convert_function_decl()` should also move
  - This completes the proto interop story for external users

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but the root cause in overload resolution may need investigation.
- `type_url` field access on Any values is not yet implemented — may revisit if additional test cases require it.
