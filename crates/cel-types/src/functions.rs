//! CEL built-in functions and macros with documentation and type information.

use std::collections::HashSet;
use std::sync::LazyLock;

use crate::CelType;

/// How a function can be called.
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionKind {
    /// Only callable as a standalone function: `int(x)`, `bool(x)`
    Standalone,
    /// Only callable as a method on specific receiver types: `"foo".endsWith("o")`
    Method(&'static [CelType]),
    /// Callable both as standalone and as method: `size(x)` or `x.size()`
    Both(&'static [CelType]),
}

/// Documentation for a CEL built-in function.
#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    /// Function name (e.g., "size")
    pub name: &'static str,
    /// Function signature (e.g., "(list<T>) -> int")
    pub signature: &'static str,
    /// Description from CEL spec
    pub description: &'static str,
    /// Optional example usage
    pub example: Option<&'static str>,
    /// How this function can be called (standalone, method, or both)
    pub kind: FunctionKind,
}

/// All CEL built-in functions with documentation.
pub static BUILTINS: &[BuiltinFunction] = &[
    // ==================== Type Conversions ====================
    BuiltinFunction {
        name: "bool",
        signature: "(value) -> bool",
        description: "Type conversion to bool. Strings: \"true\" → true, \"false\" → false. Integers: non-zero → true, zero → false.",
        example: Some("bool(1) == true"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "bytes",
        signature: "(string) -> bytes",
        description: "Converts a string to bytes using UTF-8 encoding.",
        example: Some("bytes(\"hello\")"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "double",
        signature: "(value) -> double",
        description: "Type conversion to double. Accepts int, uint, string (parsed as float), or double.",
        example: Some("double(\"3.14\") == 3.14"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "duration",
        signature: "(string) -> google.protobuf.Duration",
        description: "Parses a duration string. Format: sequence of decimal numbers with time unit suffix (h, m, s, ms, us, ns).",
        example: Some("duration(\"1h30m\")"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "dyn",
        signature: "(value) -> dyn",
        description: "Converts a value to the dyn type, enabling dynamic dispatch. Useful for heterogeneous collections.",
        example: Some("dyn(x)"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "int",
        signature: "(value) -> int",
        description: "Type conversion to int (64-bit signed). Accepts double (truncates), uint, string (parses), bool (1/0), or timestamp (Unix seconds).",
        example: Some("int(\"42\") == 42"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "string",
        signature: "(value) -> string",
        description: "Type conversion to string. Works with all primitive types, timestamps, durations, bytes (UTF-8 decode).",
        example: Some("string(123) == \"123\""),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "timestamp",
        signature: "(string) -> google.protobuf.Timestamp",
        description: "Parses an RFC 3339 formatted timestamp string.",
        example: Some("timestamp(\"2023-01-15T10:30:00Z\")"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "type",
        signature: "(value) -> type",
        description: "Returns the runtime type of a value as a type value.",
        example: Some("type(1) == int"),
        kind: FunctionKind::Standalone,
    },
    BuiltinFunction {
        name: "uint",
        signature: "(value) -> uint",
        description: "Type conversion to uint (64-bit unsigned). Accepts double (truncates), int, string (parses), or bool (1/0).",
        example: Some("uint(\"42\") == 42u"),
        kind: FunctionKind::Standalone,
    },

    // ==================== Size & Existence ====================
    BuiltinFunction {
        name: "size",
        signature: "(string|bytes|list|map) -> int",
        description: "Returns the length of a string (in Unicode code points), bytes (byte length), list (element count), or map (entry count).",
        example: Some("size(\"hello\") == 5"),
        kind: FunctionKind::Both(&[CelType::String, CelType::Bytes, CelType::List, CelType::Map]),
    },
    BuiltinFunction {
        name: "has",
        signature: "(field) -> bool",
        description: "Macro that tests whether a field is present. Returns true if the field exists and is set, false otherwise. Does not evaluate the field.",
        example: Some("has(msg.optional_field)"),
        kind: FunctionKind::Standalone,
    },

    // ==================== Macros (Comprehensions) ====================
    BuiltinFunction {
        name: "all",
        signature: "(list, iter_var, predicate) -> bool",
        description: "Macro that tests whether all elements in a list satisfy the predicate. Returns true for empty lists. Short-circuits on first false.",
        example: Some("[1, 2, 3].all(x, x > 0) == true"),
        kind: FunctionKind::Method(&[CelType::List, CelType::Map]),
    },
    BuiltinFunction {
        name: "exists",
        signature: "(list, iter_var, predicate) -> bool",
        description: "Macro that tests whether any element in a list satisfies the predicate. Returns false for empty lists. Short-circuits on first true.",
        example: Some("[1, 2, 3].exists(x, x == 2) == true"),
        kind: FunctionKind::Method(&[CelType::List, CelType::Map]),
    },
    BuiltinFunction {
        name: "exists_one",
        signature: "(list, iter_var, predicate) -> bool",
        description: "Macro that tests whether exactly one element in a list satisfies the predicate. Does not short-circuit.",
        example: Some("[1, 2, 3].exists_one(x, x == 2) == true"),
        kind: FunctionKind::Method(&[CelType::List, CelType::Map]),
    },
    BuiltinFunction {
        name: "filter",
        signature: "(list, iter_var, predicate) -> list",
        description: "Macro that returns a new list containing only elements that satisfy the predicate.",
        example: Some("[1, 2, 3, 4].filter(x, x % 2 == 0) == [2, 4]"),
        kind: FunctionKind::Method(&[CelType::List, CelType::Map]),
    },
    BuiltinFunction {
        name: "map",
        signature: "(list, iter_var, transform) -> list",
        description: "Macro that returns a new list with each element transformed by the given expression.",
        example: Some("[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
        kind: FunctionKind::Method(&[CelType::List, CelType::Map]),
    },

    // ==================== String Functions ====================
    BuiltinFunction {
        name: "contains",
        signature: "(string, substring) -> bool",
        description: "Returns true if the string contains the substring.",
        example: Some("\"hello world\".contains(\"world\") == true"),
        kind: FunctionKind::Method(&[CelType::String, CelType::List, CelType::Map]),
    },
    BuiltinFunction {
        name: "endsWith",
        signature: "(string, suffix) -> bool",
        description: "Returns true if the string ends with the given suffix.",
        example: Some("\"hello.txt\".endsWith(\".txt\") == true"),
        kind: FunctionKind::Method(&[CelType::String]),
    },
    BuiltinFunction {
        name: "matches",
        signature: "(string, regex) -> bool",
        description: "Returns true if the string matches the RE2 regular expression. The regex must match the entire string.",
        example: Some("\"hello123\".matches(\"[a-z]+[0-9]+\") == true"),
        kind: FunctionKind::Method(&[CelType::String]),
    },
    BuiltinFunction {
        name: "startsWith",
        signature: "(string, prefix) -> bool",
        description: "Returns true if the string starts with the given prefix.",
        example: Some("\"hello world\".startsWith(\"hello\") == true"),
        kind: FunctionKind::Method(&[CelType::String]),
    },

    // ==================== Timestamp/Duration Accessors ====================
    BuiltinFunction {
        name: "getDate",
        signature: "(timestamp, timezone?) -> int",
        description: "Returns the day of month from a timestamp (1-31). Optional timezone parameter.",
        example: Some("timestamp.getDate()"),
        kind: FunctionKind::Method(&[CelType::Timestamp]),
    },
    BuiltinFunction {
        name: "getDayOfMonth",
        signature: "(timestamp, timezone?) -> int",
        description: "Returns the day of month from a timestamp (0-30, zero-indexed). Optional timezone parameter.",
        example: Some("timestamp.getDayOfMonth()"),
        kind: FunctionKind::Method(&[CelType::Timestamp]),
    },
    BuiltinFunction {
        name: "getDayOfWeek",
        signature: "(timestamp, timezone?) -> int",
        description: "Returns the day of week from a timestamp (0-6, Sunday=0). Optional timezone parameter.",
        example: Some("timestamp.getDayOfWeek()"),
        kind: FunctionKind::Method(&[CelType::Timestamp]),
    },
    BuiltinFunction {
        name: "getDayOfYear",
        signature: "(timestamp, timezone?) -> int",
        description: "Returns the day of year from a timestamp (0-365). Optional timezone parameter.",
        example: Some("timestamp.getDayOfYear()"),
        kind: FunctionKind::Method(&[CelType::Timestamp]),
    },
    BuiltinFunction {
        name: "getFullYear",
        signature: "(timestamp, timezone?) -> int",
        description: "Returns the full year from a timestamp (e.g., 2023). Optional timezone parameter.",
        example: Some("timestamp.getFullYear()"),
        kind: FunctionKind::Method(&[CelType::Timestamp]),
    },
    BuiltinFunction {
        name: "getHours",
        signature: "(timestamp|duration, timezone?) -> int",
        description: "Returns the hour from a timestamp (0-23) or duration. Optional timezone parameter for timestamps.",
        example: Some("timestamp.getHours()"),
        kind: FunctionKind::Method(&[CelType::Timestamp, CelType::Duration]),
    },
    BuiltinFunction {
        name: "getMilliseconds",
        signature: "(timestamp|duration, timezone?) -> int",
        description: "Returns the milliseconds from a timestamp (0-999) or duration. Optional timezone parameter for timestamps.",
        example: Some("timestamp.getMilliseconds()"),
        kind: FunctionKind::Method(&[CelType::Timestamp, CelType::Duration]),
    },
    BuiltinFunction {
        name: "getMinutes",
        signature: "(timestamp|duration, timezone?) -> int",
        description: "Returns the minutes from a timestamp (0-59) or duration. Optional timezone parameter for timestamps.",
        example: Some("timestamp.getMinutes()"),
        kind: FunctionKind::Method(&[CelType::Timestamp, CelType::Duration]),
    },
    BuiltinFunction {
        name: "getMonth",
        signature: "(timestamp, timezone?) -> int",
        description: "Returns the month from a timestamp (0-11, zero-indexed). Optional timezone parameter.",
        example: Some("timestamp.getMonth()"),
        kind: FunctionKind::Method(&[CelType::Timestamp]),
    },
    BuiltinFunction {
        name: "getSeconds",
        signature: "(timestamp|duration, timezone?) -> int",
        description: "Returns the seconds from a timestamp (0-59) or duration. Optional timezone parameter for timestamps.",
        example: Some("timestamp.getSeconds()"),
        kind: FunctionKind::Method(&[CelType::Timestamp, CelType::Duration]),
    },
];

/// Pre-computed HashSet for O(1) builtin existence check.
static BUILTIN_SET: LazyLock<HashSet<&'static str>> =
    LazyLock::new(|| BUILTINS.iter().map(|f| f.name).collect());

/// Check if a name is a CEL built-in function.
pub fn is_builtin(name: &str) -> bool {
    BUILTIN_SET.contains(name)
}

/// Get documentation for a built-in function by name.
pub fn get_builtin(name: &str) -> Option<&'static BuiltinFunction> {
    BUILTINS.iter().find(|f| f.name == name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognizes_builtins() {
        assert!(is_builtin("size"));
        assert!(is_builtin("type"));
        assert!(is_builtin("has"));
        assert!(is_builtin("contains"));
    }

    #[test]
    fn rejects_non_builtins() {
        assert!(!is_builtin("foo"));
        assert!(!is_builtin("myFunction"));
        assert!(!is_builtin(""));
    }

    #[test]
    fn get_builtin_returns_docs() {
        let size = get_builtin("size").unwrap();
        assert_eq!(size.name, "size");
        assert!(size.description.contains("length"));
        assert!(size.example.is_some());
    }

    #[test]
    fn get_builtin_returns_none_for_unknown() {
        assert!(get_builtin("unknown").is_none());
    }

    #[test]
    fn all_builtins_have_docs() {
        for builtin in BUILTINS {
            assert!(!builtin.name.is_empty());
            assert!(!builtin.signature.is_empty());
            assert!(!builtin.description.is_empty());
        }
    }

    #[test]
    fn type_conversions_are_standalone() {
        let conversions = ["bool", "bytes", "double", "duration", "dyn", "int", "string", "timestamp", "type", "uint"];
        for name in conversions {
            let builtin = get_builtin(name).unwrap();
            assert_eq!(builtin.kind, FunctionKind::Standalone, "{} should be Standalone", name);
        }
    }

    #[test]
    fn string_methods_require_string_receiver() {
        let string_methods = ["endsWith", "startsWith", "matches"];
        for name in string_methods {
            let builtin = get_builtin(name).unwrap();
            match &builtin.kind {
                FunctionKind::Method(types) => {
                    assert!(types.contains(&CelType::String), "{} should accept String", name);
                }
                _ => panic!("{} should be a Method", name),
            }
        }
    }

    #[test]
    fn size_is_both() {
        let size = get_builtin("size").unwrap();
        match &size.kind {
            FunctionKind::Both(types) => {
                assert!(types.contains(&CelType::String));
                assert!(types.contains(&CelType::List));
                assert!(types.contains(&CelType::Map));
            }
            _ => panic!("size should be Both"),
        }
    }
}
