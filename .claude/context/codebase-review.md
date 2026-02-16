# cel-core Codebase Review: Structure & Rust Best Practices

*Conducted January 2026. Intended as a reference for post-conformance refactoring.*

## Overall Assessment

The codebase is well-organized for a project built incrementally. The module boundaries are clean, the public API is thoughtful, and the workspace structure is appropriate. That said, there are several areas where the structure has accrued friction points that would benefit from a refactor once conformance is complete.

---

## 1. The `FunctionImpl` Coupling Problem (Most Significant)

**File:** `crates/cel-core/src/types/decls.rs:17`

```rust
pub type FunctionImpl = Arc<dyn Fn(&[crate::eval::Value]) -> crate::eval::Value + Send + Sync>;
```

`OverloadDecl` lives in the `types` module but contains an optional `FunctionImpl` that references `eval::Value`. This creates a hard dependency from your type declaration layer to your evaluation layer. In a well-layered Rust crate, `types` should be a leaf module with no upward dependencies.

**Why it matters:** It means you can't use `FunctionDecl`/`OverloadDecl` in a context that doesn't pull in the entire eval module. If someone wanted to use just the parser+checker (e.g., for a linter or IDE), they'd still transitively depend on eval.

**Recommendation:** Split `OverloadDecl` into a type-only declaration (params, result, type_params, is_member) and a separate runtime binding in the eval module. The `Env` would pair them together. This is the single biggest structural improvement available.

---

## 2. Parallel Type Hierarchies: `CelValue` vs `Value` vs `CelType`

You have three overlapping representations:
- **`CelType`** (types/mod.rs) - compile-time type info
- **`CelValue`** (types/mod.rs) - compile-time constant values (7 variants)
- **`Value`** (eval/value.rs) - runtime values (16 variants)

`CelValue` is a strict subset of `Value` (only the literal types). The `From<CelValue> for Value` conversion confirms this. Right now `CelValue` exists mainly to represent enum constants and literal folding during checking.

**Critique:** This is defensible but worth questioning. `CelValue` occupies an awkward middle ground - it duplicates the primitive variants of `Value` but exists solely for the checker's `const_value` field on `VariableDecl`. In cel-go, constants are represented as `ref.Val` (the runtime value type) directly. You could consider either:
- Eliminating `CelValue` and using `Value` everywhere (accepting the eval dependency in types)
- Or accepting the current split as the cost of clean layering

These two recommendations are in tension - you can't fix both. I'd lean toward fixing #1 (decoupling types from eval) and keeping `CelValue` as the compromise.

---

## 3. `CelType` Enum Sprawl

`CelType` has 22 variants. This is a lot to match against. Several variants serve narrow roles:

| Variant | Usage |
|---------|-------|
| `TypeParam` | Only during checking (generic type parameters) |
| `TypeVar` | Only during checking (inference variables) |
| `Wrapper` | Only for proto wrapper types |
| `Abstract` | Rarely used |
| `Function` | Internal to checker |
| `Error` | Sentinel value |

**Critique:** In idiomatic Rust, when an enum has variants that only apply in certain phases, it's common to split it. You could have:
- `CelType` for the "public" types users interact with
- `InternalType` (or just keep them wrapped) for checker-internal types like `TypeVar`, `TypeParam`, `Function`

That said, since the checker operates directly on `CelType` and pattern matches against it, splitting would add conversion overhead. The practical recommendation is to keep the single enum but document which variants are internal-only (you partially do this with section comments already).

**The `TYPE_VAR_COUNTER` global** (`types/mod.rs:86`) is a code smell. A global `AtomicU64` for generating type variable IDs means type variable IDs are process-global and non-deterministic across runs. This is fine for correctness but bad for testing reproducibility. Consider making the counter owned by the `Checker` instead.

---

## 4. AST Representation

**File:** `crates/cel-core/src/types/ast.rs`

The AST is well-designed. `Spanned<T>` with `id`/`node`/`span` is clean. Using `Box<SpannedExpr>` for recursive positions is the right call.

**Issues:**
- **`Expr::String(String)` and `Expr::Bytes(Vec<u8>)` use owned allocations.** These are created during parsing and then never mutated. Using `Arc<str>` and `Arc<[u8]>` (like `Value` does) would make cloning the AST cheaper. Since the AST is cloned when building `Ast` and passed around, this matters.
- **`Expr::Ident(String)`, `Expr::RootIdent(String)`, field names, etc.** are all owned `String`s. If you ever need to clone the AST (which happens during macro expansion), this is a lot of allocation. String interning would help, but that's an optimization for later.
- **`Comprehension` has 5 `Box<SpannedExpr>` fields and 3 `String` fields.** This is the largest variant and it inflates the size of every `Expr` enum. In a production CEL implementation you might want to box the entire `Comprehension` payload to keep `Expr`'s size smaller: `Comprehension(Box<ComprehensionData>)`.

---

## 5. Module Organization

The module tree is:
```
cel-core/src/
  lib.rs        (facade re-exports)
  ast.rs        (Ast wrapper)
  env.rs        (Env builder - 1153 lines)
  unparser.rs
  types/        (CelType, CelValue, Expr, declarations)
  parser/       (lexer, parser, macros)
  checker/      (type checker, overloads, standard library)
  eval/         (evaluator, value, program, activation, functions)
  ext/          (string, math, encoders, optionals)
```

**Good:**
- Clean DAG: types -> parser -> checker -> eval -> env (no cycles)
- Extensions are isolated and composable
- `lib.rs` re-exports give a flat public API

**Issues:**

### 5a. `env.rs` is 1153 lines
This file does too much. It's the Env builder, the compile orchestrator, the program builder, and the abbreviation resolver. Consider splitting:
- `env.rs` - just the Env struct and builder methods
- `compile.rs` - the `compile()` and `parse_only()` orchestration
- `abbreviations.rs` - `Abbreviations` and `AbbrevError` (already a distinct concept)

### 5b. `types/` is overloaded
The `types` module contains three distinct concerns:
1. The type system (`CelType`, `CelValue`)
2. The AST (`Expr`, `SpannedExpr`, operators)
3. Declarations (`FunctionDecl`, `OverloadDecl`, `VariableDecl`)

In most Rust projects, the AST would be its own module (e.g., `ast/` at the crate root) rather than nested under `types/`. The declarations are also conceptually separate from the type system. This matters because `types/` is the foundation everything depends on - keeping it focused reduces the blast radius of changes.

### 5c. checker `check()` function explosion
```rust
pub fn check(...) -> CheckResult
pub fn check_with_proto_types(...) -> CheckResult
pub fn check_with_abbreviations(...) -> CheckResult
pub fn check_with_proto_types_and_abbreviations(...) -> CheckResult
```
This is a combinatorial explosion of options. The idiomatic Rust approach is a builder or options struct:
```rust
pub fn check(expr, config: &CheckConfig) -> CheckResult
```
where `CheckConfig` holds variables, functions, container, proto_types, abbreviations.

---

## 6. Error Types

You have 7 distinct error types across the crate:
- `ParseError`, `CompileError`, `AstError` (compile-time)
- `CheckError`, `CheckErrorKind` (type checking)
- `EvalError`, `EvalErrorKind` (runtime)
- `ValueError` (value conversion)
- `AbbrevError` (abbreviation validation)

**Critique:** None of these implement a common trait or compose into a unified hierarchy. This is mostly fine since they serve different phases, but:
- `ValueError` (eval/value.rs) is very simple - just `expected`/`found` strings. Consider making it a variant of `EvalError` instead.
- `CompileError` already wraps `ParseError` and `CheckError`, which is good. But it uses `Vec<ParseError>` / `Vec<CheckError>` directly rather than single errors, which is unusual for `Result` types in Rust. Usually you'd return the first error or collect into a dedicated diagnostics bag.

---

## 7. Performance-Relevant Patterns

### 7a. HashMap everywhere
The checker uses `HashMap<String, CelType>` for variables and `HashMap<String, FunctionDecl>` for functions. The standard library alone has ~50 functions. Consider:
- `FxHashMap` (from `rustc-hash`) for smaller, non-cryptographic hashing
- The overload resolver clones the substitutions `HashMap` on every attempt (`overload.rs:64`). For functions with many overloads, this is quadratic allocation.

### 7b. String allocation in hot paths
The checker does `format!("{}.{}", name, field)` for qualified name resolution. This runs for every member access. A string interner or `Cow<str>` approach would reduce allocation pressure.

### 7c. No recursion depth limits
Neither the parser nor checker have depth limits on recursive descent. A pathologically nested expression like `(((((((...))))))` could stack overflow. This is a correctness/safety issue, not just performance. Add a simple depth counter.

---

## 8. Eval Module Observations

### 8a. `Value` is well-designed
Good use of `Arc<str>`, `Arc<[u8]>`, `Arc<[Value]>` for cheap cloning. The `From`/`TryFrom` implementations are comprehensive and ergonomic. `BTreeMap` for maps gives deterministic iteration. Cross-type numeric equality/comparison follows the CEL spec correctly.

### 8b. `TypeValue` is stringly-typed
`TypeValue` (eval/value.rs) is just a wrapper around `Arc<str>`. Every call to `type_value()` creates a new `TypeValue` with a fresh `Arc`. Consider making the common type values (`int`, `bool`, `string`, etc.) static constants to avoid repeated allocation.

### 8c. `Value::cel_type()` returns `CelType::list(CelType::Dyn)` for all lists
This means runtime type information is lossy - a `[1, 2, 3]` reports its type as `list<dyn>` rather than `list<int>`. This is deliberate (matching cel-go), but worth documenting explicitly since it's a footgun for users who expect `cel_type()` to reflect the actual element types.

---

## 9. Extension Pattern

The extension pattern (`ext/`) is clean and easy to follow. Each extension returns `Vec<FunctionDecl>` and the `Env` merges them. No complaints on the pattern itself.

**Minor issue:** Extensions couple type declarations and implementations in the same `FunctionDecl` (via `OverloadDecl::with_impl()`). This means the standard library (`standard_library.rs`) has declaration-only overloads while extensions have declaration+impl overloads, and the eval module has to handle both cases. A unified approach would be cleaner.

---

## 10. Workspace & Crate Boundaries

The three-crate structure is appropriate:
- `cel-core` - library (the right default)
- `cel-core-proto` - optional proto interop
- `cel-core-conformance` - test harness

**Note:** `cel-core` no longer has a dependency on `prost-reflect`. Proto support is provided via trait abstractions (`ProtoTypeResolver`, `ProtoRegistry`, `MessageValue`) defined in `cel-core::eval`, with the prost-backed implementation (`ProstProtoRegistry`, `ProstMessage`) living in `cel-core-proto`. This keeps the core crate lightweight and allows alternative proto implementations.

---

## Summary: Priority-Ordered Refactoring Recommendations

1. **Decouple `OverloadDecl` from eval** - Split type declaration from runtime implementation
2. **Add recursion depth limits** to parser and checker (safety fix)
3. **Replace `check()` function explosion** with a config/builder pattern
4. **Split `env.rs`** into smaller focused modules
5. **Move AST to its own top-level module** rather than nesting under `types/`
6. ~~**Consider feature-gating `prost-reflect`** dependency~~ *(DONE - decoupled via traits)*
7. **Box the `Comprehension` payload** to reduce `Expr` enum size
8. **Use `Arc<str>` in AST string positions** instead of `String`
9. **Make `TYPE_VAR_COUNTER` owned by `Checker`** instead of global
10. **Make common `TypeValue` constants static** to avoid allocation
