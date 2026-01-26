# Roadmap Handoff

## Last Updated
2026-01-25

## Just Completed
- [x] Milestone 3: Extension library infrastructure

### Summary
Implemented the extension library infrastructure for CEL, adding type declarations for four extension libraries that can be registered with the `Env`. Extensions follow the cel-go pattern and can be added via `with_extension()` or `with_all_extensions()`.

### Key Files Added/Modified
- `crates/cel-core-common/src/extensions/` - New directory for extension modules
  - `string_ext.rs` - String extension functions
  - `math_ext.rs` - Math extension functions
  - `encoders_ext.rs` - Base64 encoder/decoder functions
  - `optionals_ext.rs` - Optional type functions
  - `mod.rs` - Re-exports all extension functions
- `crates/cel-core-common/src/decls.rs` - Moved from checker, shared declarations
- `crates/cel-core-common/src/lib.rs` - Export extensions and CelValue
- `crates/cel-core/src/env.rs` - Added `with_extension()`, `with_all_extensions()`
- `crates/cel-core-checker/src/` - Updated to import decls from common

### Extension Functions Implemented

**String Extension (`string_ext`):**
- `charAt(string, int) -> string`
- `indexOf(string, string) -> int`, `indexOf(string, string, int) -> int`
- `lastIndexOf(string, string) -> int`, `lastIndexOf(string, string, int) -> int`
- `substring(string, int) -> string`, `substring(string, int, int) -> string`
- `replace(string, string, string) -> string`, `replace(string, string, string, int) -> string`
- `split(string, string) -> list<string>`, `split(string, string, int) -> list<string>`
- `join(list<string>) -> string`, `join(list<string>, string) -> string`
- `trim(string) -> string`, `lowerAscii(string) -> string`, `upperAscii(string) -> string`
- `reverse(string) -> string`, `strings.quote(string) -> string`

**Math Extension (`math_ext`):**
- `math.greatest(...)` / `math.least(...)` - Various numeric overloads
- `math.abs(int/double/uint) -> same type`
- `math.ceil(double) -> double`, `math.floor(double) -> double`
- `math.round(double) -> double`, `math.trunc(double) -> double`
- `math.sign(int/double/uint) -> same type`
- `math.isNaN(double) -> bool`, `math.isInf(double) -> bool`, `math.isFinite(double) -> bool`
- Bitwise: `math.bitAnd`, `math.bitOr`, `math.bitXor`, `math.bitNot`, `math.bitShiftLeft`, `math.bitShiftRight`

**Encoders Extension (`encoders_ext`):**
- `base64.encode(bytes) -> string`
- `base64.decode(string) -> bytes`

**Optionals Extension (`optionals_ext`):**
- `optional.of(T) -> optional<T>`
- `optional.none() -> optional<dyn>`
- `optional.ofNonZeroValue(T) -> optional<T>`
- `hasValue(optional<T>) -> bool`
- `value(optional<T>) -> T`
- `or(optional<T>, optional<T>) -> optional<T>`
- `orValue(optional<T>, T) -> T`

### Architecture Decisions
1. **Extensions in cel-core-common**: Placed extension declarations in common crate so they can be shared between checker and future evaluator
2. **FunctionDecl/OverloadDecl in common**: Moved from checker to common to avoid circular dependencies
3. **Opt-in extensions**: Extensions are not loaded by default; use `with_all_extensions()` or individual `with_extension()` calls
4. **Type-only for now**: Extensions provide type checking but runtime evaluation is not yet implemented

## Next Up
- [ ] Conformance improvements (target 25/30 passing)
- [ ] Proto message field resolution
- [ ] Milestone 4: Evaluation (`Value` type, `Program`, `Activation`)

### Why This is Next
With extensions complete, the remaining conformance failures are mostly:
1. Proto message type support (e.g., `google.protobuf.Timestamp`)
2. Advanced type deduction edge cases
3. Evaluation-dependent tests

### Prerequisites
- Proto type support needs message field accessor resolution
- Evaluation requires `Value` enum and operator implementations

### Potential Challenges
- Proto message handling requires integration with prost/protobuf
- Error-as-value semantics during evaluation

## Open Questions
- Should proto message types be declared in standard library or registered separately?
- How should we handle `cel.bind` macro scoping with the checker? (Currently works but complex)

## Conformance Status
- 20/30 passing (unit tests)
- Failing tests mostly require proto types or evaluation
