# Roadmap Handoff

## Last Updated
2026-01-27

## Just Completed
- [x] Milestone 4: Evaluation Engine
  - `Value` type with all CEL values (Int, UInt, Double, String, Bytes, Bool, Null, List, Map, Timestamp, Duration, Type, Optional, Error)
  - `Program` compilation from checked AST
  - `Activation` trait with `MapActivation`, `HierarchicalActivation`, `SharedActivation`
  - Arithmetic, comparison, and logical operators
  - Short-circuit evaluation (`&&`, `||`, ternary)
  - Comprehension evaluation (`all`, `exists`, `exists_one`, `map`, `filter`)
  - Standard library function implementations
  - Evaluation conformance tests

### Summary
Implemented the CEL evaluation engine following the cel-go architecture pattern. The evaluator performs tree-walking evaluation of CEL expressions, supporting all standard operators, functions, and comprehensions. The `Program` type provides a compiled, thread-safe, cacheable representation that can be evaluated multiple times with different variable bindings.

### Key Files Added
- `crates/cel-core/src/eval/mod.rs` - Module exports and documentation
- `crates/cel-core/src/eval/value.rs` - `Value` enum with all CEL runtime types
- `crates/cel-core/src/eval/evaluator.rs` - Tree-walking evaluator (~1400 lines)
- `crates/cel-core/src/eval/activation.rs` - Variable binding implementations
- `crates/cel-core/src/eval/program.rs` - Compiled program wrapper
- `crates/cel-core/src/eval/functions.rs` - Function registry and implementations
- `crates/cel-core/src/eval/error.rs` - Evaluation error types

### Key Files Modified
- `crates/cel-core/src/lib.rs` - Re-exports eval types
- `crates/cel-core/src/env.rs` - Added `program()` method for creating Programs
- `crates/cel-core/Cargo.toml` - Added regex dependency
- `crates/cel-core-conformance/src/service.rs` - Added `eval()` implementation
- `crates/cel-core-conformance/tests/conformance.rs` - Added eval conformance tests
- `crates/cel-core-proto/src/lib.rs` - Added `value_to_proto()` for result conversion
- `README.md` - Updated with evaluation examples

### Architecture Decisions
1. **Tree-walking evaluator**: Simple, maintainable approach matching cel-go
2. **Error-as-value semantics**: Errors propagate as `Value::Error` rather than Result
3. **BTreeMap for maps**: Deterministic iteration order for reproducibility
4. **Arc-based sharing**: Cheap cloning for values, thread-safe program sharing
5. **Hierarchical activation**: Supports scoped variable bindings for comprehensions

### What the Evaluator Supports
- All literal types: null, bool, int, uint, double, string, bytes
- Collections: lists, maps with heterogeneous keys
- Operators: arithmetic (+, -, *, /, %), comparison (<, <=, ==, !=, >=, >), logical (&&, ||, !)
- Short-circuit evaluation for &&, ||, and ternary
- Field access and indexing (with optional variants)
- Function calls via FunctionRegistry
- Comprehensions: all, exists, exists_one, map, filter
- Standard library: size, contains, startsWith, endsWith, matches, type(), in

## Next Up
- [ ] Milestone 5: Full Conformance
  - Timestamp and duration arithmetic
  - Extension library runtime implementations (string_ext, math_ext, etc.)
  - Error-as-value semantics refinement
  - All conformance tests passing
  - LSP integration with new checker

### Remaining Conformance Gaps
1. **block_ext.textproto** - Requires `cel.block` macro support
2. **enums.textproto** - Requires `proto.getExt`/`proto.hasExt` functions
3. **macros2.textproto** - Some advanced macro edge cases
4. **proto2_ext.textproto** - Proto2 extension support
5. **type_deduction.textproto** - Advanced type parameter inference cases
6. **Timestamp/duration arithmetic** - Currently parses/checks but not all operations evaluate

### Potential Challenges
- Extension function implementations need runtime support (not just type declarations)
- Timestamp/duration arithmetic requires careful overflow handling
- Some conformance tests may require proto message construction at runtime

## Open Questions
- Should extension function implementations be lazy-loaded or bundled?
- How should proto message construction work at runtime without full protobuf support?
