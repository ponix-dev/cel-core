# CEL Conformance Test Failures Report

**Generated:** 2026-01-27
**Total Failing Tests:** 841

## Summary by Test File

| Test File | Failures | Notes |
|-----------|----------|-------|
| test_basic_eval | 1 | Bytes UTF-8 encoding |
| test_block_ext | 37 | `cel.block` extension not implemented |
| test_block_ext_eval | 37 | `cel.block` extension not implemented |
| test_comparisons_eval | 47 | Various comparison issues |
| test_conversions_eval | 16 | Bytes/string encoding, int range |
| test_dynamic_eval | 104 | `google.protobuf.Value` handling |
| test_encoders_ext_eval | 4 | `base64` namespace not implemented |
| test_fields_eval | 10 | Proto field access issues |
| test_lists_eval | 8 | List operation issues |
| test_logic_eval | 2 | Logic operation issues |
| test_macros2 | 19 | `cel.bind` macro issues |
| test_macros2_eval | 22 | `cel.bind` macro issues |
| test_macros_eval | 2 | Macro issues |
| test_math_ext_eval | 182 | Many math functions missing |
| test_namespace_eval | 6 | Namespace resolution issues |
| test_optionals_eval | 60 | Optional value handling |
| test_parse_eval | 53 | Parse/escape sequence issues |
| test_proto2_eval | 41 | Proto2-specific handling |
| test_proto2_ext | 18 | `proto.hasExt/getExt` not implemented |
| test_proto2_ext_eval | 18 | `proto.hasExt/getExt` not implemented |
| test_proto3_eval | 16 | Proto3-specific handling |
| test_string_eval | 1 | String operation issue |
| test_string_ext_eval | 84 | Many string functions missing/broken |
| test_timestamps_eval | 2 | Timestamp handling |
| test_type_deduction | 5 | Type deduction issues |
| test_type_deduction_check | 22 | Type checking issues |
| test_type_deduction_eval | 2 | Type deduction issues |
| test_wrappers_eval | 22 | Wrapper type handling |

## Categorized by Root Cause

### 1. Missing Extensions (Priority: High)

#### `cel.block` Extension (74 failures)
- **Files:** test_block_ext, test_block_ext_eval
- **Issue:** The `cel.block`, `cel.index`, `cel.iterVar` functions are not implemented
- **Example:** `cel.block([1, cel.index(0) + 1], cel.index(1))`

#### `base64` Namespace (4 failures)
- **Files:** test_encoders_ext_eval
- **Issue:** `base64.encode` and `base64.decode` need namespace support (currently only `base64_encode`/`base64_decode`)
- **Example:** `base64.encode(b'hello')` returns "unknown identifier: base64"

#### `proto.hasExt/getExt` Extension (36 failures)
- **Files:** test_proto2_ext, test_proto2_ext_eval
- **Issue:** Proto2 extension field access not implemented
- **Example:** `proto.hasExt(msg, cel.expr.conformance.proto2.int32_ext)`

#### Missing Math Functions (182 failures)
- **Files:** test_math_ext_eval
- **Missing functions:**
  - `math.ceil`, `math.floor`, `math.round`, `math.trunc`
  - `math.sign`
  - `math.isInf`, `math.isNaN`, `math.isFinite`
  - `math.copysign`
- **Example:** `math.ceil(1.2)` returns error

### 2. Well-Known Type Handling (Priority: High)

#### `google.protobuf.Value` (75+ failures)
- **Files:** test_dynamic_eval, test_wrappers_eval
- **Issue:** Cannot convert CEL values to `google.protobuf.Value` fields
- **Example:** `TestAllTypes{single_value: 'foo'}` fails with "expected google.protobuf.Value, got string"

#### `google.protobuf.Struct` (failures included above)
- **Issue:** Cannot convert CEL maps to `google.protobuf.Struct`
- **Example:** `TestAllTypes{single_struct: {'one': 1}}` fails with "expected google.protobuf.Struct, got map"

#### `google.protobuf.Any` (failures included above)
- **Issue:** Nested Any encoding is incorrect
- **Example:** `TestAllTypes{single_any: TestAllTypes{single_int32: 1}}` encodes incorrectly

### 3. Bytes/String Encoding (Priority: Medium)

#### UTF-8 Handling (62 failures)
- **Files:** test_basic_eval, test_conversions_eval, test_parse_eval
- **Issues:**
  - Bytes with values >127 are being UTF-8 encoded when they shouldn't be
  - `string(bytes)` not validating UTF-8 properly
  - Escape sequences in strings not handled correctly
- **Examples:**
  - `b'\377'` serializes as `[195, 191]` instead of `[255]`
  - `string(b'\000\xff')` should error but returns a string

### 4. String Extension Functions (Priority: Medium)

#### Missing/Broken Functions (84 failures)
- **Files:** test_string_ext_eval
- **Issues:**
  - `split()` with limit parameter
  - `replace()` with limit parameter
  - `substring()` behavior
  - `trim()` with Unicode whitespace
  - `format()` function
  - `quote()` function
  - `join()` on lists

### 5. Proto Field Behavior (Priority: Medium)

#### `has()` Function (25 failures)
- **Issues:**
  - `has(proto.undefined_field)` should error, returns false
  - `has()` on enum fields set to default value returns false (should be true)
  - `has()` on oneof fields set to default returns false (should be true)

#### Null/Empty Field Handling (16 failures)
- **Issues:**
  - Setting field to null doesn't clear it properly
  - Empty wrapper types should return null, return 0
  - Proto equality with null fields

### 6. Optional Value Handling (Priority: Medium)

#### Optional Operations (60 failures)
- **Files:** test_optionals_eval
- **Issue:** Various optional value behaviors not matching spec

### 7. Int/Double Range Checking (Priority: Low)

#### Range Errors (5 failures)
- **Issue:** `int()` conversion from large doubles should error
- **Example:** `int(1e99)` should error but returns max int64

### 8. Type Checking Issues (Priority: Low)

#### Type Parameter Resolution (27 failures)
- **Files:** test_type_deduction, test_type_deduction_check
- **Issue:** Generic type parameter resolution in checker
- **Example:** `tuple` type not recognized

#### Wrapper Type Promotion (3 failures)
- **Issue:** Wrapper types not properly assignable

## Recommended Fix Priority

1. **Well-Known Types (Value/Struct/Any)** - Blocks many proto tests
2. **Bytes Encoding** - Affects proto serialization correctness
3. **Math Extension** - Large number of failures, relatively easy
4. **String Extension** - Large number of failures
5. **cel.block Extension** - Popular optimization extension
6. **base64 Namespace** - Simple fix (add namespace alias)
7. **Proto has() Behavior** - Correctness issue
8. **proto.hasExt/getExt** - Proto2-specific feature
