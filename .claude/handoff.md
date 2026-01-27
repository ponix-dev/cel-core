# Roadmap Handoff

## Last Updated
2026-01-27

## Just Completed
- [x] Timestamp and Duration Support (Milestone 5 partial)
  - Full timestamp parsing from RFC 3339 strings (with timezone offsets)
  - Full duration parsing from CEL duration strings (e.g., "1h30m15s")
  - Timestamp/duration arithmetic with proper overflow and range checking
  - All timestamp accessor methods (getFullYear, getMonth, getDate, etc.)
  - All duration accessor methods (getHours, getMinutes, getSeconds, getMilliseconds)
  - Timezone support via IANA names and UTC offset strings
  - Proper range validation (timestamps: year 0001-9999, durations: ~10000 years)

### Summary
Implemented comprehensive timestamp and duration support for the CEL evaluator. This includes parsing, formatting, arithmetic operations, and all accessor methods. The implementation follows CEL spec requirements for range validation, ensuring that timestamps stay within the valid range (year 0001 to 9999) and durations don't exceed approximately 10000 years.

### Key Files Added
- `crates/cel-core/src/eval/time.rs` - 435 lines of timestamp/duration utilities
  - `parse_timestamp()` - RFC 3339 parsing with timezone support
  - `parse_duration()` - CEL duration string parsing (h, m, s, ms, us, ns)
  - `format_timestamp()` / `format_duration()` - String conversion
  - `TimestampComponent` enum for accessor methods
  - `parse_timezone()` for IANA and offset timezone handling

### Key Files Modified
- `crates/cel-core/src/eval/value.rs` - Added range constants and validation methods
  - `Timestamp::MIN_SECONDS` / `MAX_SECONDS` for valid range
  - `Duration::MIN_SECONDS` / `MAX_SECONDS` for valid range
  - `is_valid()` methods for range checking
  - Chrono conversion helpers
- `crates/cel-core/src/eval/evaluator.rs` - Added timestamp/duration operations
  - Arithmetic with overflow/range checking
  - `timestamp()` and `duration()` type conversion functions
  - All timestamp/duration accessor methods
- `crates/cel-core/src/eval/error.rs` - Added `range_error()` constructor
- `crates/cel-core/src/eval/mod.rs` - Exported time module
- `crates/cel-core/Cargo.toml` - Added chrono and chrono-tz dependencies

### Conformance Test Results
- **Timestamp tests: 73/76 passing** (up from 71/76)
- Fixed: Duration range validation for timestamp span calculations
- Remaining 3 failures require proto type support (not timestamp/duration logic)

## Next Up
- [ ] Proto type conformance issues
  - `google.protobuf.Timestamp == type(timestamp(...))` comparison
  - `google.protobuf.Duration == type(duration(...))` comparison
  - Proto object value support for `getMilliseconds()` test

### Why This Is Next
The remaining timestamp conformance failures are all proto-related:
1. Type comparison with `google.protobuf.Timestamp` and `google.protobuf.Duration`
2. Object value handling for proto message inputs

These are the logical next step as they're blocking full timestamp/duration conformance.

### Prerequisites
- Need to understand how cel-go handles `type()` for proto well-known types
- May need to special-case Timestamp/Duration type comparisons

### Potential Challenges
- Proto type names may need special handling in the type system
- Object value support requires proto message construction at runtime

## Open Questions
- Should `type(timestamp(...))` return `google.protobuf.Timestamp` or `timestamp`?
- How should proto message values be constructed without full protobuf support?
