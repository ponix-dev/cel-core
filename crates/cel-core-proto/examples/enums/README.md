# Enum Handling in CEL

CEL supports two modes for handling protobuf enum values:

| Mode | Setting | Enum Representation | `type()` Returns |
|------|---------|--------------------|--------------------|
| **Strong** (default) | `Env::with_standard_library()` | `Value::Enum` with type name | Enum type name (e.g., `my.package.Color`) |
| **Legacy** | `.with_legacy_enums()` | `Value::Int` | `"int"` |

## Strong Enums (Default)

Strong enum typing is the CEL spec's intended direction. Enum values carry their fully-qualified type name and support:

- **Type introspection**: `type(x)` returns the enum type name
- **Enum constructors**: `MyEnum(1)` (from int) or `MyEnum("VALUE_NAME")` (from string)
- **Int conversion**: `int(enum_value)` extracts the numeric value

```
GlobalEnum.GAZ = cel.expr.conformance.proto3.GlobalEnum(2)
type(GlobalEnum.GAZ) = type(cel.expr.conformance.proto3.GlobalEnum)
TestAllTypes.NestedEnum(1) = cel.expr.conformance.proto3.TestAllTypes.NestedEnum(1)
TestAllTypes.NestedEnum('BAZ') = cel.expr.conformance.proto3.TestAllTypes.NestedEnum(2)
int(GlobalEnum.GAZ) = 2
```

## Legacy Enums

Legacy mode returns enum values as plain integers, matching older CEL behavior. This is useful for backward compatibility.

```
GlobalEnum.GAZ = 2
type(GlobalEnum.GAZ) = type(int)
GlobalEnum.GAR == 1 = true
TestAllTypes.NestedEnum.BAR + 1 = 2
```

## When to Use Each Mode

- **Strong enums**: New projects, full CEL spec compliance, type-safe enum handling
- **Legacy enums**: Backward compatibility with systems expecting integer enum values

## Run the Examples

```bash
cargo run -p cel-core --example enums_legacy
cargo run -p cel-core --example enums_strong
```
