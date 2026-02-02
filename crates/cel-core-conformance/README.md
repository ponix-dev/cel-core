# CEL Conformance Testing

This crate validates cel-core against the official [cel-spec](https://github.com/google/cel-spec) conformance test suite. CEL-Core currently passes **100% of all conformance tests**.

## What is tested

The conformance suite runs all **29 test files** from `cel-spec/tests/simple/testdata/` across three phases:

| Phase | Description |
|-------|-------------|
| **Parse + Check** | Verifies every expression parses successfully and passes type checking |
| **Type Check** | Verifies the type checker deduces the correct result types (for `typed_result` tests) |
| **Eval** | Verifies evaluation produces the expected value, error, or unknown result |

### Test files

| File | Tests | Description |
|------|-------|-------------|
| basic | 43 | Fundamental CEL operations |
| bindings_ext | 8 | `cel.bind` extension |
| block_ext | 37 | `cel.block` extension |
| comparisons | 406 | Equality, ordering, and cross-type comparisons |
| conversions | 109 | Type conversions (int, uint, double, string, etc.) |
| dynamic | 226 | Dynamic dispatch and runtime type handling |
| encoders_ext | 4 | Base64 encoding extension |
| enums | 85 | Proto enum handling (legacy and strong modes) |
| fields | 60 | Proto field access and qualified identifiers |
| fp_math | 30 | Floating-point arithmetic |
| integer_math | 64 | Integer arithmetic and overflow |
| lists | 39 | List operations |
| logic | 30 | Boolean logic and short-circuit evaluation |
| macros | 44 | `has`, `all`, `exists`, `exists_one`, `map`, `filter` |
| macros2 | 46 | Additional macro tests |
| math_ext | 199 | Math extension functions |
| namespace | 14 | Container-scoped name resolution |
| optionals | 70 | Optional types extension |
| parse | 219 | Parser edge cases and expression syntax |
| plumbing | 5 | Infrastructure and identity tests |
| proto2 | 108 | Proto2 message handling |
| proto2_ext | 18 | Proto2 extensions |
| proto3 | 75 | Proto3 message handling |
| string | 51 | String operations |
| string_ext | 216 | String extension functions |
| timestamps | 76 | Timestamp and duration operations |
| type_deduction | 47 | Type checker deduction verification |
| unknowns | 0 | Placeholder (no tests defined in cel-spec) |
| wrappers | 36 | Proto wrapper type handling |

### Enum handling

The `enums.textproto` file contains four sections that require two different evaluation modes:

- **Legacy mode** (`legacy_proto2`, `legacy_proto3`): Enum values represented as plain integers
- **Strong mode** (`strong_proto2`, `strong_proto3`): Enum values represented as typed `EnumValue` messages

Both modes are tested. Notably, cel-go currently skips the strong enum sections entirely.

## How it works

### Test loading

Test files are `.textproto` files conforming to the `SimpleTestFile` proto message from cel-spec. Each file contains sections, and each section contains individual test cases. The loader (`src/loader.rs`) uses `prost-reflect` to parse the text-format protobuf files.

### Test execution

Each test case specifies:

- **`expr`**: The CEL expression to test
- **`bindings`**: Variable values for evaluation
- **`type_env`**: Type declarations (variables and functions)
- **`container`**: Namespace for qualified name resolution
- **`result_matcher`**: The expected outcome — a value, typed result, eval error, or unknown

The test harness runs each case through the `ConformanceService` trait, which wraps the full `cel-core` `Env` API.

### CEL spec flags

Individual tests use flags defined by the cel-spec proto:

- **`disable_check`**: Skip type checking (test still runs parse and eval phases)
- **`check_only`**: Skip evaluation (test verifies only parse and type check phases)

These flags are respected exactly as the spec intends.

### Baseline tracking

Conformance results are tracked in `conformance-baseline.md` within this crate. The `conformance-report.sh` script compares current results against this baseline to detect regressions and improvements.

## Running the tests

### Prerequisites

- Rust toolchain
- [mise](https://mise.jdx.dev/)

### Setup

Initialize the cel-spec submodule (one-time):

```bash
mise run conformance:setup
```

### Run tests

```bash
mise run conformance:test
```

### Generate a conformance report

Runs all tests, compares against the baseline, outputs a markdown summary, and updates the stored baseline:

```bash
mise run conformance:report
```
