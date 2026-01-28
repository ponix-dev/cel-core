# Namespace Resolution for Protobuf Types

CEL provides two mechanisms to simplify working with fully-qualified protobuf type names:

| Feature | Purpose | Protobuf Analogy |
|---------|---------|------------------|
| **Container** | Set default package context | `package myapp.models;` |
| **Abbreviations** | Import specific types | `import "google/protobuf/timestamp.proto";` |

## Name Resolution Order

When cel-core encounters a type name (e.g., `Timestamp` in `Timestamp{seconds: 123}`), it resolves the name in this order:

### 1. Container Hierarchy (C++ Namespace Rules)

If a container is set, cel-core walks up the package hierarchy. For container `a.b.c` and name `Foo`:

```
a.b.c.Foo  →  a.b.Foo  →  a.Foo  →  Foo
```

The first match wins. This mirrors how protobuf resolves type references in `.proto` files.

### 2. Abbreviations Map

If no container match is found, cel-core checks the abbreviations map for an explicit short name → fully qualified name mapping.

### 3. Fully Qualified Name

Finally, the name is used as-is (assumed to already be fully qualified).

## Resolution During Compilation vs Evaluation

Name resolution happens at **two stages**:

### Compile Time (Type Checking)

When you call `env.compile("Timestamp{seconds: 123}")`:

1. The checker resolves `Timestamp` using containers/abbreviations
2. The fully qualified name (`google.protobuf.Timestamp`) is stored in the AST's reference map
3. Type information is validated against the proto registry

### Evaluation Time

When you call `program.eval(&activation)`:

1. The evaluator first checks the reference map for pre-resolved names from type checking
2. If not found, it re-applies container/abbreviation resolution
3. The proto registry constructs the actual message

This two-stage approach means type checking catches errors early, and evaluation uses the pre-resolved names for efficiency.

## Containers

```rust
let env = Env::with_standard_library()
    .with_proto_types(registry)
    .with_container("google.protobuf");

// "Timestamp" resolves to "google.protobuf.Timestamp"
env.compile("Timestamp{seconds: 123}")
```

## Abbreviations

```rust
let abbrevs = Abbreviations::new()
    .add("google.protobuf.Timestamp")?
    .add("google.protobuf.Duration")?;

let env = Env::with_standard_library()
    .with_proto_types(registry)
    .with_abbreviations(abbrevs);

// "Timestamp" resolves via abbreviation
env.compile("Timestamp{seconds: 123}")
```

## Using Both Together

```rust
let abbrevs = Abbreviations::from_qualified_names(&[
    "google.protobuf.Timestamp",
    "google.protobuf.Duration",
])?;

let env = Env::with_standard_library()
    .with_proto_types(registry)
    .with_container("myapp.models")      // Domain types resolve here first
    .with_abbreviations(abbrevs);        // Well-known types via abbreviation
```

With this setup:
- `User` → tries `myapp.models.User` first (container)
- `Timestamp` → not in `myapp.models`, falls through to abbreviation → `google.protobuf.Timestamp`

## Run the Examples

```bash
cargo run -p cel-core --example containers
cargo run -p cel-core --example abbreviations
cargo run -p cel-core --example combined
```
