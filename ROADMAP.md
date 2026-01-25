# CEL Implementation Roadmap

This document outlines the path to achieving full CEL (Common Expression Language) implementation parity with [cel-go](https://github.com/google/cel-go), the reference implementation.

## Current State

### Workspace Structure

```
crates/
  cel-core-parser/       # Lexer + parser, produces AST
  cel-core-proto/        # Bidirectional AST <-> proto conversion
  cel-core-lsp/          # Language Server Protocol implementation
  cel-core-conformance/  # Conformance testing against cel-spec
```

### What We Have

| Component | Status | Notes |
|-----------|--------|-------|
| **Lexer** | Complete | Logos-based, all CEL tokens |
| **Parser** | Complete | Hand-written recursive descent, error recovery |
| **AST** | Complete | All expression types, span tracking |
| **Proto Conversion** | Partial | AST to/from `ParsedExpr`, no `CheckedExpr` |
| **LSP** | Partial | Diagnostics, hover, semantic tokens |
| **Validation** | Partial | Arity checking, receiver types (literals only) |
| **Type Checking** | Not started | No `CheckedExpr` production |
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

**Current status:** Not implemented. We have partial validation in `cel-core-lsp` for LSP diagnostics, but it:
- Only infers types for literals (not expressions)
- Doesn't produce `CheckedExpr`
- Doesn't handle generics (`List<T>`, `Map<K,V>`)
- Doesn't resolve function overloads

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

### Phase 2: Type System

**Goal:** Full type inference and `CheckedExpr` production.

#### 2.1 Enhanced Type Representation ✅

Implemented parameterized types in a new `cel-core-types` crate:

```rust
// Current
pub enum CelType {
    List,
    Map,
    // ...
}

// Enhanced
pub enum CelType {
    List(Box<CelType>),           // List<T>
    Map(Box<CelType>, Box<CelType>), // Map<K, V>
    Type(Box<CelType>),           // type(T) - type values
    Message(String),              // Proto message type
    Enum(String),                 // Proto enum type
    // ...
}
```

#### 2.2 Type Checker Implementation

Create a new `cel-core-checker` crate (or module in `cel-core-lsp`) that:

1. **Walks the AST** assigning types to each node
2. **Resolves identifiers** against the type environment
3. **Resolves function overloads** based on argument types
4. **Produces `CheckedExpr`** with `type_map` and `reference_map`

```rust
pub struct Checker {
    type_env: HashMap<String, CelType>,
    type_map: HashMap<i64, CelType>,
    reference_map: HashMap<i64, Reference>,
}

impl Checker {
    pub fn check(&mut self, parsed: &ParsedExpr) -> Result<CheckedExpr, Vec<Issue>>;
}
```

#### 2.3 Function Overload Resolution

Many CEL functions are overloaded:
- `+`: `int + int`, `double + double`, `string + string`, `list + list`, `duration + timestamp`, etc.
- `size`: `size(string)`, `size(list)`, `size(map)`, `size(bytes)`

The checker must select the correct overload based on argument types and record it in `reference_map`.

#### 2.4 Integration with LSP

The existing LSP validation should use the new checker for:
- More accurate error messages
- Type information for hover
- Better completion suggestions

---

### Phase 3: Compiled Program

**Goal:** Create a reusable, cacheable compiled representation.

#### 3.1 Internal Program Representation

```rust
pub struct Program {
    /// The checked expression (or parsed, for unchecked eval)
    expr: CompiledExpr,
    /// Resolved types for each node
    types: HashMap<i64, CelType>,
    /// Resolved function implementations
    functions: HashMap<i64, FunctionImpl>,
}

impl Program {
    /// Compile from a checked expression (preferred)
    pub fn from_checked(checked: &CheckedExpr) -> Result<Self, CompileError>;

    /// Compile from a parsed expression (unchecked, less efficient)
    pub fn from_parsed(parsed: &ParsedExpr) -> Result<Self, CompileError>;
}
```

#### 3.2 Optimization Opportunities

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

#### 4.4 Built-in Function Implementations

~50 built-in functions need implementations:

| Category | Functions |
|----------|-----------|
| Type conversion | `int`, `uint`, `double`, `string`, `bytes`, `bool`, `type`, `dyn` |
| String | `size`, `contains`, `startsWith`, `endsWith`, `matches`, `charAt`, `indexOf`, `lastIndexOf`, `replace`, `split`, `join`, `substring`, `trim`, `lower`, `upper` |
| Collections | `size`, `in` |
| Timestamp/Duration | `getFullYear`, `getMonth`, `getDate`, `getHours`, `getMinutes`, `getSeconds`, `getMilliseconds`, `getDayOfWeek`, `getDayOfYear` |
| Macros | `has`, `all`, `exists`, `exists_one`, `map`, `filter` |

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

## Crate Structure (Proposed)

```
crates/
  cel-core-parser/       # Lexer, parser, AST (with node IDs)
  cel-core-types/        # Type system, CelType, CelValue (new)
  cel-core-checker/      # Type checking, produces CheckedExpr (new)
  cel-core-eval/         # Program compilation and evaluation (new)
  cel-core-proto/        # Proto conversion (enhanced)
  cel-core-lsp/          # LSP implementation (uses above)
  cel-core-conformance/  # Conformance testing
```

Alternatively, `cel-core-types`, `cel-core-checker`, and `cel-core-eval` could be a single `cel-core-runtime` crate.

---

## Milestones

### Milestone 1: Basic Evaluation
- [ ] Value type with primitives
- [ ] Evaluator for literals
- [ ] Arithmetic operators
- [ ] Comparison operators
- [ ] Logical operators (with short-circuit)
- [ ] Variable binding resolution
- [ ] Pass: basic literal conformance tests

### Milestone 2: Collections and Strings
- [ ] List and map value support
- [ ] Collection operators (`in`, indexing)
- [ ] String built-in functions
- [ ] Pass: string and collection conformance tests

### Milestone 3: Type Checking
- [x] Node IDs in parser
- [x] Parameterized types
- [ ] Type inference through expressions
- [ ] Function overload resolution
- [ ] `CheckedExpr` production
- [ ] Integration with LSP

### Milestone 4: Macros and Comprehensions
- [x] Macro expansion in parser
- [ ] Comprehension evaluation
- [x] `all`, `exists`, `exists_one`, `map`, `filter`
- [x] Pass: comprehension conformance tests

### Milestone 5: Full Conformance
- [ ] Timestamp and duration support
- [ ] Proto message integration
- [ ] Error semantics
- [ ] All conformance tests passing

---

## References

- [CEL Spec](https://github.com/google/cel-spec) - Official specification and test suite
- [cel-go](https://github.com/google/cel-go) - Reference implementation
- [CEL Language Definition](https://github.com/google/cel-spec/blob/master/doc/langdef.md)
- [CEL-Go Codelab](https://codelabs.developers.google.com/codelabs/cel-go) - Tutorial showing the phases
