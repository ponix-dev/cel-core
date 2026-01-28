# Roadmap Handoff

## Last Updated
2026-01-28

## Just Completed
- [x] Abbreviations support for type name shortcuts (Milestone 3 completion)
  - New `Abbreviations` type for mapping short names to fully-qualified names
  - Integration with checker for type resolution during compilation
  - Integration with evaluator for runtime type resolution
  - Examples demonstrating containers, abbreviations, and combined usage

### Summary
Implemented abbreviations support for CEL namespace resolution. This allows short names (like `Timestamp`) to be used instead of fully-qualified names (like `google.protobuf.Timestamp`) in CEL expressions. The feature complements the existing container support, following cel-go's namespace resolution patterns.

### Key Files Added
- `crates/cel-core/examples/namespaces/abbreviations.rs` - Example using abbreviations
- `crates/cel-core/examples/namespaces/containers.rs` - Example using containers
- `crates/cel-core/examples/namespaces/combined.rs` - Example combining both features
- `crates/cel-core/examples/namespaces/README.md` - Documentation for namespace resolution

### Key Files Modified
- `crates/cel-core/src/env.rs` - Added `Abbreviations` type and `with_abbreviations()` builder
- `crates/cel-core/src/checker/checker.rs` - Added abbreviation expansion during type checking
- `crates/cel-core/src/checker/mod.rs` - Exported new checker functions with abbreviations
- `crates/cel-core/src/eval/evaluator.rs` - Added abbreviation support during evaluation
- `crates/cel-core/src/eval/program.rs` - Pass abbreviations to evaluator

### Notable Decisions
- Abbreviations are resolved after container hierarchy (C++ namespace rules)
- Reference map stores fully-qualified names for efficient evaluation
- Abbreviations work for both message types and enum values

## Next Up
- [ ] Error-as-value semantics
  - CEL treats errors as first-class values that propagate through expressions
  - Some operations should absorb errors (e.g., `false && error` returns `false`)

- [ ] Proto type conformance
  - `type(timestamp(...))` should return `google.protobuf.Timestamp`
  - Wrapper type comparison with proto types

### Prerequisites
- Need to audit all operators for error propagation rules
- Review cel-spec for exact error semantics

### Potential Challenges
- Error propagation rules vary by operator
- Balancing performance with correct error handling

## Open Questions
- Should `type(timestamp(...))` return `google.protobuf.Timestamp` or the CEL native `timestamp` type?
- How should optional field errors be handled in error-as-value semantics?
