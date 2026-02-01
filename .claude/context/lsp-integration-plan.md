# LSP + cel-core Integration: Opportunities & Recommendations

## Current State

The LSP currently has its **own parallel validation layer** (`types/validation.rs`, `types/builtins.rs`) that duplicates a lot of what cel-core's `Env` + checker already provides. It uses a `VariableResolver` trait with two hardcoded implementations:

- **EmptyResolver** — no variables, only builtins (used for `.cel` files)
- **ProtovalidateResolver** — adds `this`, `rules`, `now` (used for proto files)

Meanwhile, cel-core has evolved to include a full `Env` builder with type checking, overload resolution, proto type registry, extensions, abbreviations, and more. The LSP doesn't use any of this — it calls `cel_core::parse()` directly and runs its own validation pass on the raw AST.

---

## 1. Replace the LSP's Custom Validation with cel-core's Checker

**What exists now:** The LSP has ~500 lines of custom validation in `types/validation.rs` and `types/builtins.rs` that check for undefined variables, arity mismatches, and basic type errors. This is a subset of what cel-core's checker does, and it will fall further behind as cel-core evolves.

**What to do:** Switch from `cel_core::parse()` + custom validation to `env.compile()` (or `env.check()` on the parsed AST). The checker already produces:
- `CheckResult.errors` — type errors, undefined references, arity mismatches
- `CheckResult.type_map` — inferred type for every expression node (by ID)
- `CheckResult.reference_map` — resolved references with overload IDs and constant values

This would immediately give richer diagnostics and eliminate the maintenance burden of the parallel validation system.

**Migration path:** The LSP's `VariableResolver` trait maps directly to `Env` configuration:
- `resolver.resolve_variable(name)` → `env.with_variable(name, type)`
- `resolver.get_functions()` → `env.with_function(decl)` / `env.with_extension(...)`
- The protovalidate resolver's `this`, `rules`, `now` → `env.with_variable(...)` calls

The hover system could then use `CheckResult.type_map` to show inferred types for any expression, not just builtins.

---

## 2. Settings-Driven Custom Environments via `settings.toml`

**What to do:** Add a `settings.toml` (or similar) that users can place in their workspace root to configure the CEL environment. The LSP would read this on startup and on file change, then build an `Env` from it.

**Proposed schema:**

```toml
[env]
container = "my.package.name"
extensions = ["strings", "math", "encoders", "optionals"]  # or "all"
strong_enums = true  # default

[env.variables]
request = "my.api.Request"       # message type
user_id = "string"
count = "int"
items = "list(string)"
metadata = "map(string, dyn)"

[env.abbreviations]
# short name -> fully qualified name
qualified_names = [
  "my.api.Request",
  "my.api.Response",
]

[[env.functions]]
name = "customValidate"
overloads = [
  { id = "customValidate_string", params = ["string"], result = "bool", member = false },
]

# File descriptor sets for proto type resolution
[[env.proto]]
path = "descriptors/api.binpb"   # FileDescriptorSet binary

[[env.proto]]
path = "descriptors/common.binpb"
```

**Implementation steps:**
1. Add a `settings.rs` module to `cel-core-lsp` that deserializes `settings.toml` using `toml` + `serde`
2. Parse the type strings (e.g. `"list(string)"`) into `CelType` values — write a small parser or reuse a subset of cel-core's type representation
3. Build an `Env` from the settings on startup and cache it
4. Watch the settings file for changes and rebuild the `Env`
5. Fall back to a default `Env::with_standard_library()` when no settings file exists

---

## 3. File Descriptor Set Support

**What exists now:** cel-core already has `ProtoTypeRegistry` with `add_file_descriptor_set(bytes)` that accepts raw `FileDescriptorSet` protobuf bytes. The conformance service demonstrates the pattern.

**What to do:** Let users point to `.binpb` files (compiled `FileDescriptorSet`) in their settings. The LSP would:

1. Read the binary file(s) from the paths in `settings.toml`
2. Create a `ProtoTypeRegistry`, add descriptor sets in dependency order
3. Pass it to `Env::with_proto_types(registry)`

This enables:
- Field access type checking on message types (`request.user_id` resolves to `string`)
- Enum constant resolution (`Status.ACTIVE` resolves to the enum value)
- Struct literal validation (`MyMessage{field: value}` checks field names and types)
- Container-scoped name resolution (qualified names relative to `container`)

**Bonus:** The LSP could also accept descriptor sets via an LSP `workspace/configuration` request, so IDE extensions can pass them programmatically (e.g., from a buf/protoc build step).

---

## 4. Richer Hover Information Using Type Map

**What exists now:** Hover shows function documentation from a hardcoded builtin registry, or error messages.

**What to do:** With `CheckResult.type_map`, the LSP can show the **inferred type of any expression** on hover:

- Hover over `x + 1` → `int` (if `x: int`)
- Hover over `request.items` → `list(string)` (from proto descriptor)
- Hover over `items.map(i, i.size())` → `list(int)`
- Hover over a function call → show the resolved overload signature, not just the generic docs

The `reference_map` also tells you which specific overload was selected, so you could show `_+_(int, int) -> int` rather than the generic `_+_` docs.

---

## 5. Per-File Environment Overrides

**[Suggestion]** Beyond a global `settings.toml`, consider supporting per-file or per-directory overrides. For example:

- A `cel-env.toml` in a subdirectory that adds variables specific to that context
- Multiple named environments in settings.toml that map to file globs:

```toml
[[profiles]]
name = "api-validation"
glob = "protos/api/**/*.proto"
variables = { request = "my.api.Request" }

[[profiles]]
name = "policy"
glob = "policies/**/*.cel"
variables = { resource = "my.Resource", principal = "my.Principal" }
```

This is useful when a project has CEL expressions in different contexts (API validation, policy evaluation, etc.) that need different variable bindings.

---

## 6. Extension Library Toggle

**What exists now:** The LSP's builtin registry is static and doesn't include extension functions (strings, math, encoders, optionals).

**What to do:** With `settings.toml` driving the `Env`, users could enable/disable extension libraries:

```toml
extensions = ["strings", "math"]  # only these two
```

Mapped to:
```rust
env.with_extension(cel_core::ext::string_extension())
   .with_extension(cel_core::ext::math_extension())
```

This means `"hello".upperAscii()` would get proper type checking and hover docs when the strings extension is enabled, and an "undefined method" error when it's not — matching actual runtime behavior.

---

## 7. Protovalidate Integration Improvements

**What exists now:** The protovalidate resolver hardcodes `this`, `rules`, `now` as `dyn` types.

**What to do:** If the user provides file descriptor sets, the LSP could resolve the *actual types* of `this` and `rules` from the proto context:

- Parse the surrounding proto field/message to determine which message type `this` refers to
- Look up the corresponding `validate` rule type for `rules`
- Set `this` to `CelType::Message("my.api.Request")` instead of `CelType::Dyn`

This would enable field-level autocompletion and type checking within protovalidate expressions — `this.email.isEmail()` would verify that `email` is actually a string field.

---

## 8. Go-to-Definition and Find References

**[Suggestion]** With `reference_map` from the checker, you have the information needed for basic navigation:

- **Go-to-definition** on a variable → jump to its declaration (in settings.toml or the proto file)
- **Go-to-definition** on a function → jump to its declaration
- **Find references** for a variable across all CEL expressions in the workspace

This is a longer-term feature but the type checker already produces the data needed.

---

## 9. Diagnostic Severity and Warnings

**[Suggestion]** Currently all diagnostics are `ERROR` severity. With the checker's richer output, you could introduce warnings:

- **Warning** for `dyn` typed expressions (loss of type safety)
- **Warning** for deprecated functions or patterns
- **Info** for type narrowing suggestions (e.g., "this expression is always true")

---

## 10. LSP Configuration via `workspace/configuration`

**[Suggestion]** In addition to `settings.toml`, support the standard LSP `workspace/configuration` request. This lets IDE extensions pass settings dynamically:

```json
{
  "cel": {
    "variables": { "x": "int" },
    "extensions": ["strings", "math"],
    "descriptorSets": ["path/to/descriptors.binpb"]
  }
}
```

This is the standard way IDEs like VS Code pass settings to language servers. The `settings.toml` would serve as the file-based fallback for CLI/non-IDE usage.

---

## Summary of Effort

| Item | Scope | Key cel-core APIs |
|------|-------|-------------------|
| Replace custom validation with checker | Medium — core refactor of diagnostics pipeline | `Env::compile()`, `CheckResult` |
| `settings.toml` configuration | Medium — new module, deserialization, Env building | `Env` builder methods |
| File descriptor set loading | Small — read files, feed to registry | `ProtoTypeRegistry::add_file_descriptor_set()` |
| Richer hover from type map | Small — use existing CheckResult data | `CheckResult.type_map`, `reference_map` |
| Per-file environment profiles | Medium — glob matching, profile merging | `Env` builder |
| Extension library toggle | Small — conditional `with_extension()` calls | `string_extension()`, `math_extension()`, etc. |
| Typed protovalidate context | Medium — proto file analysis for `this`/`rules` types | `ProtoTypeRegistry::get_message()` |
| LSP workspace/configuration | Small — tower-lsp configuration handler | N/A (LSP protocol) |

The highest-impact change is item 1 (replacing the custom validation with cel-core's checker). Everything else builds naturally on top of that, since the `Env` becomes the single configuration point that settings, descriptor sets, and extensions all feed into.
