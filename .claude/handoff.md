# Roadmap Handoff

## Last Updated
2026-01-26

## Just Completed
- [x] Proto type registry for message field resolution
- [x] Unified `Ast` type with proto roundtrip support
- [x] AST unparser (to_cel_string)
- [x] Container support for qualified name resolution
- [x] Conformance tests: 24/30 files passing (up from 20/30)

### Summary
Added proto type support for CEL type checking, enabling resolution of protobuf message fields, enum values, and qualified type names. Also added a unified `Ast` type following the cel-go pattern with proto roundtrip support, and an unparser for converting ASTs back to CEL source text.

### Key Files Added/Modified
- `crates/cel-core-common/src/proto_types.rs` - NEW: `ProtoTypeRegistry` for resolving protobuf types
- `crates/cel-core/src/ast.rs` - NEW: Unified `Ast` type with `to_checked_expr()`, `from_checked_expr()`, `to_cel_string()`
- `crates/cel-core/src/unparser.rs` - NEW: Convert AST back to CEL source text
- `crates/cel-core-proto/src/checked_expr.rs` - NEW: `to_checked_expr()` and `check_result_from_proto()` helpers
- `crates/cel-core-checker/src/checker.rs` - Added proto type resolution, container support
- `crates/cel-core/src/env.rs` - Added `CompileError`, `with_proto_types()`, `set_container()`
- `crates/cel-core-conformance/src/service.rs` - Uses proto registry for conformance tests

### What ProtoTypeRegistry Provides
- Message field type lookup (`get_field_type`)
- Enum value resolution (`get_enum_value`)
- Qualified name resolution with container scoping (`resolve_qualified`)
- Well-known type mapping (Timestamp -> CelType::Timestamp, Duration -> CelType::Duration, wrappers)
- Support for nested message/enum types

### What Unified Ast Provides
- `Ast::is_checked()` - Check if AST has been type-checked
- `Ast::result_type()` - Get the result type of the expression
- `Ast::to_checked_expr()` - Convert to proto CheckedExpr
- `Ast::to_parsed_expr()` - Convert to proto ParsedExpr
- `Ast::from_checked_expr()` - Create from proto CheckedExpr (roundtrip)
- `Ast::from_parsed_expr()` - Create from proto ParsedExpr
- `Ast::to_cel_string()` - Convert back to CEL source text

### Architecture Decisions
1. **ProtoTypeRegistry in cel-core-common**: Placed in common crate for sharing between checker and conformance
2. **Descriptor pool integration**: Uses prost_reflect::DescriptorPool for efficient proto type lookup
3. **Container scoping**: Checker tracks container (e.g., "cel.expr.conformance.proto3") for qualified name resolution
4. **Well-known type mapping**: google.protobuf.Timestamp/Duration map to native CelType variants

## Next Up
- [ ] Milestone 4: Evaluation (`Value` type, `Program`, `Activation`)
- [ ] Fix remaining conformance failures (6 files: block_ext, enums, macros2, proto2_ext, type_deduction)

### Remaining Conformance Failures
1. **block_ext.textproto** - Requires `cel.block` macro support
2. **enums.textproto** - Requires `proto.getExt`/`proto.hasExt` functions
3. **macros2.textproto** - Some advanced macro edge cases
4. **proto2_ext.textproto** - Proto2 extension support (`proto.getExt`, `proto.hasExt`)
5. **type_deduction.textproto** - Advanced type parameter inference cases (flexible_type_parameter_assignment, wrappers)

### Prerequisites for Next Steps
- Evaluation requires `Value` enum and operator implementations
- `proto.getExt`/`proto.hasExt` need special handling for proto2 extensions

### Potential Challenges
- Type deduction failures involve subtle type parameter binding rules
- Proto2 extensions are a different model than proto3 fields
- Error-as-value semantics during evaluation

## Open Questions
- Should `cel.block` be a macro or a special form?
- How should proto2 extensions be declared vs proto3 fields?

## Conformance Status
- 24/30 passing (parse + check tests)
- Major categories passing: basic, comparisons, conversions, dynamic, fields, fp_math, integer_math, lists, logic, macros, namespace, optionals, parse, plumbing, proto2, proto3, string, timestamps, type_deduction_check (partial), unknowns
