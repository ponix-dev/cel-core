# CEL Implementation Roadmap

This document outlines the path to achieving full CEL (Common Expression Language) implementation parity with [cel-go](https://github.com/google/cel-go), the reference implementation.

## Current State

### Workspace Structure

```
crates/
  cel-core-common/       # Shared types: CelType, CelValue, AST
  cel-core-parser/       # Lexer + parser, produces AST
  cel-core-checker/      # Type checker, produces CheckedExpr
  cel-core-proto/        # Bidirectional AST <-> proto conversion
  cel-core/              # Unified Env API (cel-go pattern)
  cel-core-lsp/          # Language Server Protocol implementation
  cel-core-conformance/  # Conformance testing against cel-spec
```

### What We Have

| Component | Status | Notes |
|-----------|--------|-------|
| **Lexer** | Complete | Logos-based, all CEL tokens |
| **Parser** | Complete | Hand-written recursive descent, error recovery |
| **AST** | Complete | All expression types, span tracking, node IDs |
| **Proto Conversion** | Complete | AST to/from `ParsedExpr` and `CheckedExpr` |
| **Type Checker** | Partial | `cel-core-checker` crate, standard library, 15/30 conformance files pass |
| **LSP** | Partial | Diagnostics, hover, semantic tokens |
| **Unified Env** | Complete | `cel-core` crate with `Env` struct (cel-go pattern) |
| **Program** | Not started | No compiled/cacheable representation |
| **Evaluation** | Not started | No runtime execution |

---

## CEL Processing Phases

CEL follows a three-phase model, matching cel-go's architecture:

```
┌─────────┐     ┌─────────┐     ┌─────────┐     ┌─────────┐
│  Parse  │ --> │  Check  │ --> │ Program │ --> │  Eval   │
└─────────┘     └─────────┘     └─────────┘     └─────────┘
     │               │               │               │
     v               v               v               v
 ParsedExpr    CheckedExpr      Compiled        Value
                              (cached, reusable)
```

### Phase 1: Parse

**Input:** Source text (`&str`)
**Output:** `ParsedExpr` (proto) or internal AST

The parser converts CEL source into an abstract syntax tree. Each node receives a unique ID that later phases use to attach metadata (types, references).

**Current status:** Complete. Our parser produces a full AST with spans, and `cel-core-proto` converts it to `ParsedExpr`.

**Gap:** Our internal AST doesn't preserve node IDs across conversions. The proto conversion assigns IDs during conversion, not during parsing.

### Phase 2: Check (Type Checking)

**Input:** `ParsedExpr` + type environment (`TypeDecl[]`)
**Output:** `CheckedExpr`

The checker validates the expression against a type environment and produces a `CheckedExpr` containing:

- **`type_map`**: Maps node IDs to resolved types
- **`reference_map`**: Maps node IDs to resolved declarations (which function overload, which variable)
- **`expr`**: The original parsed expression
- **`source_info`**: Source location data

**Current status:** Partially implemented in `cel-core-checker` crate:
- ✅ Type inference for all expression types
- ✅ Parameterized types (`List<T>`, `Map<K,V>`)
- ✅ Function overload resolution
- ✅ `CheckedExpr` production via `cel-core-proto`
- ✅ Standard library with ~50 function declarations
- ❌ Extension libraries (string_ext, math_ext, etc.)
- ❌ Proto message field resolution

### Phase 3: Program (Compilation)

**Input:** `CheckedExpr` (or `ParsedExpr` for unchecked evaluation)
**Output:** Internal executable representation

cel-go compiles the checked expression into a `Program` that is:
- **Stateless** - no mutable state
- **Thread-safe** - can be shared across threads
- **Cacheable** - compile once, evaluate many times

This is the key insight: **parsing and checking are expensive, evaluation is cheap**. The `Program` caches all the work from earlier phases.

**Current status:** Not implemented. We don't have an internal compiled representation.

### Phase 4: Eval (Evaluation)

**Input:** `Program` + variable bindings (`Activation`)
**Output:** `Value` (or error)

The evaluator walks the compiled program with concrete variable values and produces a result.

**Current status:** Not implemented. No evaluator, no `Value` type, no operator implementations.

---

## Unified Env Architecture (cel-go Parity)

cel-go uses a central `Env` struct that coordinates all phases. This is the recommended pattern for CEL implementations:

```
┌─────────────────────────────────────────────────────────────┐
│                         Env                                  │
│  - Variable declarations (types)                            │
│  - Function declarations (standard lib + extensions)        │
│  - Parser config (macros)                                   │
│  - Checker config                                           │
├─────────────────────────────────────────────────────────────┤
│  env.parse(source) ──→ Ast                                  │
│  env.check(ast) ──→ Ast (with types)                        │
│  env.compile(source) ──→ Ast (parse + check combined)       │
│  env.program(ast) ──→ Program (executable, cacheable)       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                       Program                                │
│  - Stateless, thread-safe, cacheable                        │
│  - Contains compiled expression + resolved functions        │
├─────────────────────────────────────────────────────────────┤
│  program.eval(bindings) ──→ Value                           │
└─────────────────────────────────────────────────────────────┘
```

### Current State vs cel-go

| cel-go | Ours | Status |
|--------|------|--------|
| `Env` (unified coordinator) | `cel_core::Env` | ✅ Complete |
| `env.Parse()` | `env.parse()` | ✅ Complete |
| `env.Check()` | `env.check()` | ✅ Complete |
| `env.Compile()` | `env.compile()` | ✅ Complete |
| `env.Program()` | None | ❌ Missing |
| `program.Eval()` | None | ❌ Missing |
| Extension libraries | `with_extension()`, `with_all_extensions()` | ✅ Complete |

### Proposed Unified Env

```rust
pub struct Env {
    type_env: TypeEnv,            // Variable + function declarations
    parser_options: ParseOptions, // Macro config, etc.
}

impl Env {
    /// Create with standard library
    pub fn new() -> Self;

    /// Add extension libraries
    pub fn with_string_extension(self) -> Self;
    pub fn with_math_extension(self) -> Self;
    pub fn with_optional_extension(self) -> Self;

    /// Add variable declarations
    pub fn with_variable(self, name: &str, cel_type: CelType) -> Self;

    /// Parse source to AST
    pub fn parse(&self, source: &str) -> Result<Ast, Issues>;

    /// Type check an AST
    pub fn check(&self, ast: &Ast) -> Result<CheckedAst, Issues>;

    /// Parse + check combined
    pub fn compile(&self, source: &str) -> Result<CheckedAst, Issues>;

    /// Create executable program
    pub fn program(&self, ast: &CheckedAst) -> Result<Program, Error>;
}

pub struct Program {
    expr: CompiledExpr,
    functions: HashMap<String, FunctionImpl>,
}

impl Program {
    /// Evaluate with variable bindings
    pub fn eval(&self, bindings: &Activation) -> Result<Value, Error>;
}
```

### Extension Libraries

Extensions are optional function libraries that can be added to an `Env`:

| Extension | Functions | Test File |
|-----------|-----------|-----------|
| `string_ext` | `charAt`, `indexOf`, `substring`, `reverse`, `format` | `string_ext.textproto` |
| `math_ext` | `math.greatest`, `math.least` | `math_ext.textproto` |
| `encoders_ext` | `base64.encode`, `base64.decode` | `encoders_ext.textproto` |
| `optional_ext` | `optional.of`, `optional.none`, `optional.ofNonZeroValue` | `optionals.textproto` |
| `bindings_ext` | `cel.bind` | `bindings_ext.textproto` |
| `block_ext` | `cel.block` | `block_ext.textproto` |

Extensions are registered at `Env` creation time:

```rust
let env = Env::new()
    .with_string_extension()
    .with_math_extension()
    .with_variable("request", CelType::message("http.Request"));

let program = env.compile("request.path.substring(0, 5)")?;
let result = program.eval(&activation)?;
```

---

## Implementation Plan

### Phase 1: Parser Enhancements

**Goal:** Ensure parser output can support all downstream phases.

#### 1.1 Node IDs in AST ✅

~~Currently, node IDs are assigned during proto conversion.~~ Node IDs are now assigned during parsing via `Parser.next_id`. Each AST node has a unique `id: i64` field:

```rust
// Current
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

// Enhanced
pub struct Spanned<T> {
    pub id: i64,        // Unique node ID
    pub node: T,
    pub span: Span,
}
```

This enables the `type_map` and `reference_map` in `CheckedExpr` to reference our AST nodes directly.

#### 1.2 Macro Expansion ✅

CEL macros (`all`, `exists`, `exists_one`, `map`, `filter`) are now expanded during parsing via a configurable `MacroRegistry`:

- **Option A was implemented:** Macros expand to `Comprehension` nodes at parse time
- `MacroRegistry` keys macros by `name:arg_count:is_receiver` (matching cel-go)
- 16 standard macros: `has`, `all`, `exists`, `exists_one`, `map`, `filter`, `transformList`, `transformMap`
- `ParseOptions` and `parse_with_options()` enable custom macro configuration
- `macro_calls` map preserved for IDE features (hover shows original syntax)

#### 1.3 Optional Syntax Support ✅

CEL has optional entry syntax for lists, maps, and structs:
```cel
[1, ?maybe_value, 3]        // Optional list element
{"a": 1, ?"b": maybe_val}   // Optional map entry
Type{field: val, ?opt: x}   // Optional struct field
```

Our AST now tracks the `optional` flag via dedicated wrapper types:
- `ListElement { expr, optional }` for list elements
- `MapEntry { key, value, optional }` for map entries
- `StructField { name, value, optional }` for struct fields

The proto converter correctly handles `optional_indices` for lists and `optional_entry` for map/struct entries.

**Note:** This is distinct from optional chaining (`x.?y`, `x[?key]`), which is not yet implemented.

---

### Phase 2: Type System ✅

**Goal:** Full type inference and `CheckedExpr` production.

#### 2.1 Enhanced Type Representation ✅

Implemented parameterized types in `cel-core-common` crate:

```rust
pub enum CelType {
    // Primitives
    Bool, Int, UInt, Double, String, Bytes,
    // Parameterized
    List(Arc<CelType>),
    Map(Arc<CelType>, Arc<CelType>),
    Type(Arc<CelType>),
    // Proto types
    Message(Arc<str>),
    Enum(Arc<str>),
    // Type checking
    TypeParam(Arc<str>),  // Generic type parameter
    TypeVar(u64),         // Inference variable
    // ...
}
```

#### 2.2 Type Checker Implementation ✅

Implemented in `cel-core-checker` crate:

```rust
pub struct Checker<'env> {
    env: &'env mut TypeEnv,
    type_map: HashMap<i64, CelType>,
    reference_map: HashMap<i64, ReferenceInfo>,
    errors: Vec<CheckError>,
    substitutions: HashMap<Arc<str>, CelType>,
}

pub fn check(expr: &SpannedExpr, env: &mut TypeEnv) -> CheckResult;
```

Features implemented:
- ✅ Type inference for all expression types
- ✅ Function overload resolution with type parameter substitution
- ✅ `type_map` and `reference_map` production
- ✅ Scoped type environments for comprehensions
- ✅ Standard library with ~50 function declarations

#### 2.3 Checker Options (cel-go parity)

cel-go provides configurable checker behavior via `CheckerOption`. Key options to support:

| cel-go Option | Description | Status |
|--------------|-------------|--------|
| `HomogeneousAggregateLiterals` | Heterogeneous collections produce errors | ❌ Not yet |
| `CrossTypeNumericComparisons` | Allow int/uint/double comparisons | ❌ Not yet |
| `ValidatedDeclarations` | Pre-validated type declarations | ❌ Not yet |

#### 2.4 Function Overload Resolution ✅

Implemented in `cel-core-checker/src/overload.rs`:
- Type parameter binding (`T`, `K`, `V`)
- Best-match selection for multiple overloads
- Substitution tracking across expressions

#### 2.5 Integration with LSP

The existing LSP validation should use the new checker for:
- More accurate error messages
- Type information for hover
- Better completion suggestions

**Status:** Not yet integrated. LSP still uses older validation.

---

### Phase 3: Unified Env + Program

**Goal:** Create a unified `Env` coordinator and cacheable `Program` representation.

#### 3.1 Unified Env

Create `cel-core` crate (or extend existing) with unified environment:

```rust
pub struct Env {
    type_env: TypeEnv,
    parser_options: ParseOptions,
    function_impls: HashMap<String, FunctionImpl>,
}

impl Env {
    pub fn new() -> Self;
    pub fn with_string_extension(self) -> Self;
    pub fn with_math_extension(self) -> Self;
    pub fn with_variable(self, name: &str, cel_type: CelType) -> Self;

    pub fn parse(&self, source: &str) -> Result<Ast, Issues>;
    pub fn check(&self, ast: &Ast) -> Result<CheckedAst, Issues>;
    pub fn compile(&self, source: &str) -> Result<CheckedAst, Issues>;
    pub fn program(&self, ast: &CheckedAst) -> Result<Program, Error>;
}
```

#### 3.2 Program Representation

```rust
pub struct Program {
    /// The checked expression
    expr: CompiledExpr,
    /// Resolved types for each node
    types: HashMap<i64, CelType>,
    /// Resolved function implementations
    functions: HashMap<i64, Arc<dyn Fn(&[Value]) -> Value>>,
}

impl Program {
    /// Evaluate with variable bindings
    pub fn eval(&self, activation: &dyn Activation) -> Result<Value, EvalError>;
}
```

Key properties (matching cel-go):
- **Stateless** - no mutable state
- **Thread-safe** - can be shared via `Arc<Program>`
- **Cacheable** - compile once, evaluate many times

#### 3.3 Optimization Opportunities

The compiled program can include optimizations:
- **Constant folding**: `1 + 2` -> `3`
- **Short-circuit markers**: `&&` and `||` evaluation
- **Pre-resolved field accessors**: For proto messages

---

### Phase 4: Evaluation

**Goal:** Execute programs and produce values.

#### 4.1 Value Type

```rust
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Double(f64),
    String(Arc<str>),
    Bytes(Arc<[u8]>),
    List(Arc<[Value]>),
    Map(Arc<HashMap<Value, Value>>),
    Timestamp(DateTime<Utc>),
    Duration(chrono::Duration),
    Type(CelType),
    // Error values (CEL uses error-as-value semantics)
    Error(CelError),
}
```

#### 4.2 Activation (Variable Bindings)

```rust
pub trait Activation {
    fn resolve(&self, name: &str) -> Option<Value>;
}

// Simple implementation
pub struct MapActivation {
    bindings: HashMap<String, Value>,
}
```

#### 4.3 Evaluator

```rust
pub struct Evaluator;

impl Evaluator {
    pub fn eval(&self, program: &Program, activation: &dyn Activation) -> Result<Value, EvalError>;
}
```

The evaluator handles:
- **Operator dispatch**: Arithmetic, comparison, logical, membership
- **Function calls**: Built-in and extension functions
- **Short-circuit evaluation**: `&&`, `||`, ternary
- **Comprehensions**: `all`, `exists`, `map`, `filter`
- **Error propagation**: CEL's error-as-value semantics

#### 4.4 Standard Library Functions

The standard library (~50 functions) is always available:

| Category | Functions |
|----------|-----------|
| Type conversion | `int`, `uint`, `double`, `string`, `bytes`, `bool`, `type`, `dyn` |
| String (standard) | `size`, `contains`, `startsWith`, `endsWith`, `matches` |
| Collections | `size`, `in`, indexing, field access |
| Timestamp/Duration | `getFullYear`, `getMonth`, `getDate`, `getHours`, etc. |
| Macros | `has`, `all`, `exists`, `exists_one`, `map`, `filter` |

#### 4.5 Extension Libraries

Extensions are optional and added via `Env` configuration:

| Extension | Functions | Status |
|-----------|-----------|--------|
| `string_ext` | `charAt`, `indexOf`, `lastIndexOf`, `substring`, `replace`, `split`, `join`, `trim`, `lower`, `upper`, `reverse`, `format` | ❌ |
| `math_ext` | `math.greatest`, `math.least` | ❌ |
| `encoders_ext` | `base64.encode`, `base64.decode` | ❌ |
| `optional_ext` | `optional.of`, `optional.none`, `optional.ofNonZeroValue`, `value`, `hasValue`, `or`, `orValue`, `optMap`, `optFlatMap` | ❌ |
| `bindings_ext` | `cel.bind` | ❌ |
| `block_ext` | `cel.block` | ❌ |

Extensions are implemented as functions that return `Vec<FunctionDecl>`:

```rust
// In cel-core-checker/src/extensions/strings.rs
pub fn string_extension() -> Vec<FunctionDecl> {
    vec![
        FunctionDecl::new("charAt")
            .with_overload(
                "string_char_at_int",
                &[CelType::String, CelType::Int],
                CelType::String,
                true, // is_receiver
            ),
        FunctionDecl::new("indexOf")
            .with_overload(
                "string_index_of_string",
                &[CelType::String, CelType::String],
                CelType::Int,
                true,
            ),
        // ...
    ]
}
```

---

### Phase 5: Conformance Testing

**Goal:** Pass the official cel-spec conformance test suite.

The `cel-core-conformance` crate already has the infrastructure:

```rust
pub trait ConformanceService {
    fn parse(&self, source: &str) -> ParseResponse;
    fn check(&self, parsed: &ParsedExpr, type_env: &[TypeDecl]) -> CheckResponse;
    fn eval(&self, expr: &ParsedExpr, bindings: &[Binding]) -> EvalResponse;
}
```

#### 5.1 Test Categories

The cel-spec test suite covers:
- Basic literals and operators
- String operations
- List and map operations
- Comparisons and equality
- Logic operators (short-circuit)
- Macros and comprehensions
- Timestamps and durations
- Type conversions
- Proto message handling
- Error semantics

#### 5.2 Incremental Progress

Track conformance by category, starting with:
1. Literals and basic operators
2. String functions
3. Collections
4. Macros
5. Timestamps/durations
6. Proto integration

---

## Crate Structure

```
crates/
  cel-core-common/       # Shared types: CelType, CelValue, AST       ✅ Complete
  cel-core-parser/       # Lexer, parser (depends on common)          ✅ Complete
  cel-core-checker/      # Type checking, standard library            ✅ Partial
  cel-core-proto/        # Bidirectional proto conversion             ✅ Complete
  cel-core/              # Unified Env API (cel-go pattern)           ✅ Complete
  cel-core-lsp/          # LSP implementation                         ✅ Partial
  cel-core-conformance/  # Conformance testing                        ✅ Complete
```

The `cel-core` crate will be the main entry point, re-exporting key types and providing the unified `Env` API:

```rust
// User-facing API
use cel_core::{Env, Program, Value};

let env = Env::new()
    .with_variable("name", CelType::String);

let program = env.compile("name.startsWith('test')")?;
let result = program.eval(&[("name", Value::String("test123".into()))])?;
```

---

## Milestones

### Milestone 1: Parser ✅
- [x] Lexer with all CEL tokens
- [x] Recursive descent parser with error recovery
- [x] Node IDs in AST
- [x] Macro expansion (`all`, `exists`, `map`, `filter`, etc.)
- [x] Optional syntax support

### Milestone 2: Type Checker ✅
- [x] Parameterized types (`List<T>`, `Map<K,V>`)
- [x] Type inference through expressions
- [x] Function overload resolution
- [x] `CheckedExpr` production
- [x] Standard library (~50 functions)
- [x] Conformance tests: 15/30 files passing

### Milestone 3: Unified Env + Extensions
- [x] Create `cel-core` crate with unified `Env`
- [x] Extension library infrastructure (`with_extension()`, `with_all_extensions()`)
- [x] `string_ext` - `charAt`, `indexOf`, `lastIndexOf`, `substring`, `replace`, `split`, `join`, `trim`, `lowerAscii`, `upperAscii`, `reverse`, `strings.quote`
- [x] `math_ext` - `math.greatest`, `math.least`, `math.abs`, `math.ceil`, `math.floor`, `math.round`, `math.sign`, `math.isNaN`, `math.isInf`, `math.isFinite`, `math.bitAnd`, `math.bitOr`, `math.bitXor`, `math.bitNot`, `math.bitShiftLeft`, `math.bitShiftRight`
- [x] `encoders_ext` - `base64.encode`, `base64.decode`
- [x] `optional_ext` - `optional.of`, `optional.none`, `optional.ofNonZeroValue`, `hasValue`, `value`, `or`, `orValue`
- [x] Moved `decls.rs` to `cel-core-common` for shared use
- [x] Proto type registry for message field resolution
- [x] Unified `Ast` type with proto roundtrip support
- [x] AST unparser (to_cel_string)
- [x] Container support for qualified name resolution
- [x] Abbreviations support for type name shortcuts
- [x] Conformance tests: 24/30 files passing

### Milestone 4: Evaluation
- [x] `Value` type with all CEL values (Int, UInt, Double, String, Bytes, Bool, Null, List, Map, Timestamp, Duration, Type, Optional, Error)
- [x] `Program` compilation from `CheckedExpr`
- [x] `Activation` trait for variable bindings (`MapActivation`, `HierarchicalActivation`, `SharedActivation`)
- [x] Arithmetic, comparison, logical operators
- [x] Short-circuit evaluation (`&&`, `||`, ternary)
- [x] Comprehension evaluation (`all`, `exists`, `exists_one`, `map`, `filter`)
- [x] String function implementations (standard library)
- [x] Conformance tests: evaluation verification

### Milestone 5: Full Conformance

**Goal:** Pass all official cel-spec conformance tests. Current state: 33/59 test suites passing, ~617 individual test failures remaining.

- [x] Timestamp and duration support
- [x] Proto message field resolution
- [x] Error-as-value semantics

#### 5.1 Evaluator Extension Libraries (~409 failures)

Register extension library function implementations in the evaluator runtime. The checker already handles these; the evaluator needs matching runtime functions.

##### 5.1a String Extension Functions (163 failures) ✅
- [x] `charAt`, `indexOf`, `lastIndexOf` — string indexing
- [x] `lowerAscii`, `upperAscii` — ASCII case conversion
- [x] `replace`, `split`, `substring`, `trim` — string manipulation
- [x] `join` — list-to-string joining
- [x] `reverse` — string reversal
- [x] `format` — printf-style string formatting (`%s`, `%d`, `%f`, `%e`, `%x`, `%o`, `%b`)
- [x] `strings.quote` — string quoting via `strings` namespace
- [x] Namespaced function dispatch in evaluator (required for `strings.quote`)

##### 5.1b Math Extension Functions (182 failures)
- [x] `math.greatest`, `math.least` — min/max across numeric types
- [x] `math.ceil`, `math.floor`, `math.round`, `math.trunc` — rounding
- [x] `math.abs`, `math.sign` — absolute value and sign
- [x] `math.isNaN`, `math.isInf`, `math.isFinite` — float classification
- [x] `math.bitAnd`, `math.bitOr`, `math.bitXor`, `math.bitNot` — bitwise ops
- [x] `math.bitShiftLeft`, `math.bitShiftRight` — bit shifting

##### 5.1c Optional Extension Functions (60 failures) ✅
- [x] `optional.of`, `optional.none`, `optional.ofNonZeroValue` — constructors via namespace
- [x] `hasValue`, `value`, `or`, `orValue` — optional value access methods
- [x] Optional chaining support (`x.?y`, `x[?key]`) in evaluator
- [x] `has()` on optional values (maps and protos)
- [x] `is_zero_value` logic for `ofNonZeroValue`
- [x] `optional_type` in type system

##### 5.1d Encoders Extension (4 failures)
- [ ] `base64.encode`, `base64.decode` — base64 encoding via namespace

#### 5.2 Conversion & Operator Edge Cases (~13 failures)

Fix standard library conversion functions and operator behavior to match spec.

- [ ] `bool()` from string: accept `"1"`, `"0"`, `"t"`, `"f"`, `"TRUE"`, `"True"`, `"FALSE"`, `"False"` (not just `"true"`/`"false"`)
- [ ] `int()` / `uint()` from out-of-range doubles: error instead of silent clamping
- [x] Map `+` operator for map merging (needed by `transformMap` macro)
- [ ] Repeated map key detection in map literals

#### 5.3 Two-Variable Macro Forms (~41 failures)

Implement two-variable comprehension macros (parser + evaluator).

- [x] `.exists(i, v, ...)` — two-variable exists with index
- [x] `.all(i, v, ...)` — two-variable all with index
- [x] `.existsOne(i, v, ...)` — two-variable existsOne
- [x] `.transformList(i, v, ...)` — list transform with index variable binding
- [x] `.transformMap(k, v, ...)` — map transform with key/value variable binding
- [x] `existsOne` camelCase alias (cel-go compatibility)
- [x] Checker: two-variable comprehension type inference (index=Int, value=element type)
- [x] Evaluator: iter_var/iter_var2 binding order fix (iter_var=index, iter_var2=element)

#### 5.4 Strong Enum Typing (~51 failures)

Represent enums as typed values rather than plain integers.

- [ ] `EnumValue` representation with fully-qualified enum type name
- [ ] Enum type conversion functions (`TestAllTypes.NestedEnum(2)`, `GlobalEnum(-33)`)
- [ ] `type()` returns enum type name instead of `int`
- [ ] Out-of-range enum value validation

#### 5.5 Well-Known Type (WKT) Handling (~24 failures)

Proper field access and value coercion for protobuf well-known types.

- [ ] Wrapper type field access (`google.protobuf.BoolValue{value: true}.value`)
- [ ] Wrapper type auto-unwrapping when assigned to `Any` or `Value` fields
- [ ] Duration/Timestamp coercion to `google.protobuf.Value`
- [ ] `google.protobuf.FieldMask` field access (`paths`)
- [ ] `google.protobuf.Empty` to `Value` coercion

#### 5.6 Any Type Support (~12 failures)

Implement `google.protobuf.Any` packing, unpacking, and comparison.

- [ ] `type_url` field access on Any values
- [ ] Any unpacking for equality comparison
- [ ] Bytewise fallback comparison for unknown Any types

#### 5.7 Namespace & Qualified Identifier Resolution (~10 failures)

Fix variable and namespace resolution edge cases.

- [ ] Qualified variable identifiers (`a.b.c` where `a.b` is the variable name)
- [ ] Container namespace shadowing and lookup
- [ ] `google.protobuf.Timestamp` / `google.protobuf.Duration` as type identifiers

#### 5.8 Type Checker Inference Improvements (~27 failures)

Improve type parameter resolution and reduce unnecessary `dyn` fallback.

- [x] Scoped type parameter resolution (prevent cross-expression type param collision)
- [x] Type parameter binding widening (Null/Dyn/TypeVar widen to concrete types)
- [x] Null assignability to message/duration/timestamp/optional/abstract parameter types
- [x] Custom function type declarations (`fn`, `tuple`, `sort` in conformance tests)
- [x] Comprehension accumulator type refinement from loop step
- [x] Type specificity-based join_types (prefer concrete types over Dyn)
- [x] Abstract type matching in overload resolution
- [x] `optional_type` proto name for Optional type conversion
- [ ] Parameterized type propagation through nested generics (`list<list<list<T>>>`)
- [ ] Wrapper type promotion in type checker (`int` → `wrapper<int>`)

#### 5.9 Proto Extensions (`proto.hasExt`/`getExt`) (~36 failures)

Implement the proto extension namespace for proto2 extension field access.

- [ ] `proto.hasExt(msg, ext)` — check if extension field is set
- [ ] `proto.getExt(msg, ext)` — get extension field value

#### 5.10 `cel.block` Extension (~74 failures)

Implement the Common Subexpression Elimination block extension.

- [ ] `cel.block(slots, expr)` — CSE optimization blocks (parser + checker + evaluator)
- [ ] `cel.index(n)` — slot reference within blocks
- [ ] `cel.iterVar` — iteration variable reference within blocks

#### 5.11 Miscellaneous Behavioral Fixes (~15 failures)

Remaining edge cases across multiple test files.

- [ ] `dyn()` type equality: different message types should not be equal
- [ ] FloatValue precision: float32 vs float64 precision loss detection
- [ ] `google.protobuf.Any` literal construction validation
- [x] Comprehension exhaustive evaluation semantics (errors don't short-circuit; `exists_one` iterates all elements)
- [ ] `has()` on explicitly-set message fields returns `true`
- [ ] `has()` on optional map entries with `optional.none()` values
- [ ] Deeply nested message field access (`NestedTestAllTypes.payload.single_int64`)
- [ ] Proto map key serialization ordering (deterministic output)
- [ ] LSP integration with new checker

---

## References

- [CEL Spec](https://github.com/google/cel-spec) - Official specification and test suite
- [cel-go](https://github.com/google/cel-go) - Reference implementation
- [CEL Language Definition](https://github.com/google/cel-spec/blob/master/doc/langdef.md)
- [CEL-Go Codelab](https://codelabs.developers.google.com/codelabs/cel-go) - Tutorial showing the phases
