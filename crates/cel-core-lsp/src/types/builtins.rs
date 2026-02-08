//! CEL built-in functions and macros with documentation.

use std::collections::HashMap;
use std::sync::LazyLock;

use super::function::FunctionDef;

/// All CEL built-in functions with documentation, lazily initialized.
pub static BUILTINS: LazyLock<HashMap<&'static str, FunctionDef>> = LazyLock::new(|| {
    let defs = vec![
        // ==================== Type Conversions ====================
        FunctionDef {
            name: "bool",
            signature: "(value) -> bool",
            description: "Type conversion to bool. Accepts bool (identity) or string (\"true\" → true, \"false\" → false).",
            example: Some("bool(\"true\") == true"),
        },
        FunctionDef {
            name: "bytes",
            signature: "(value) -> bytes",
            description: "Type conversion to bytes. Accepts bytes (identity) or string (UTF-8 encoded).",
            example: Some("bytes(\"hello\")"),
        },
        FunctionDef {
            name: "double",
            signature: "(value) -> double",
            description: "Type conversion to double. Accepts int, uint, string (parsed as float), or double.",
            example: Some("double(\"3.14\") == 3.14"),
        },
        FunctionDef {
            name: "duration",
            signature: "(value) -> google.protobuf.Duration",
            description: "Type conversion to duration. Accepts duration (identity) or string (format: sequence of decimal numbers with time unit suffix h, m, s, ms, us, ns).",
            example: Some("duration(\"1h30m\")"),
        },
        FunctionDef {
            name: "dyn",
            signature: "(value) -> dyn",
            description: "Converts a value to the dyn type, enabling dynamic dispatch. Useful for heterogeneous collections.",
            example: Some("dyn(x)"),
        },
        FunctionDef {
            name: "int",
            signature: "(value) -> int",
            description: "Type conversion to int (64-bit signed). Accepts int (identity), uint, double (truncates toward zero), string (parses), or timestamp (Unix seconds).",
            example: Some("int(\"42\") == 42"),
        },
        FunctionDef {
            name: "string",
            signature: "(value) -> string",
            description: "Type conversion to string. Works with all primitive types, timestamps, durations, bytes (UTF-8 decode).",
            example: Some("string(123) == \"123\""),
        },
        FunctionDef {
            name: "timestamp",
            signature: "(value) -> google.protobuf.Timestamp",
            description: "Type conversion to timestamp. Accepts timestamp (identity) or string (RFC 3339 format).",
            example: Some("timestamp(\"2023-01-15T10:30:00Z\")"),
        },
        FunctionDef {
            name: "type",
            signature: "(value) -> type",
            description: "Returns the runtime type of a value as a type value.",
            example: Some("type(1) == int"),
        },
        FunctionDef {
            name: "uint",
            signature: "(value) -> uint",
            description: "Type conversion to uint (64-bit unsigned). Accepts uint (identity), int, double (truncates toward zero), or string (parses).",
            example: Some("uint(\"42\") == 42u"),
        },

        // ==================== Size & Existence ====================
        FunctionDef {
            name: "size",
            signature: "(string|bytes|list|map) -> int",
            description: "Returns the length of a string (in Unicode code points), bytes (byte length), list (element count), or map (entry count).",
            example: Some("size(\"hello\") == 5"),
        },
        FunctionDef {
            name: "has",
            signature: "(field) -> bool",
            description: "Macro that tests whether a field is present. Returns true if the field exists and is set, false otherwise. Does not evaluate the field.",
            example: Some("has(msg.optional_field)"),
        },

        // ==================== Macros (Comprehensions) ====================
        FunctionDef {
            name: "all",
            signature: "(list, iter_var, predicate) -> bool",
            description: "Macro that tests whether all elements in a list satisfy the predicate. Returns true for empty lists. Short-circuits on first false.",
            example: Some("[1, 2, 3].all(x, x > 0) == true"),
        },
        FunctionDef {
            name: "exists",
            signature: "(list, iter_var, predicate) -> bool",
            description: "Macro that tests whether any element in a list satisfies the predicate. Returns false for empty lists. Short-circuits on first true.",
            example: Some("[1, 2, 3].exists(x, x == 2) == true"),
        },
        FunctionDef {
            name: "exists_one",
            signature: "(list, iter_var, predicate) -> bool",
            description: "Macro that tests whether exactly one element in a list satisfies the predicate. Does not short-circuit.",
            example: Some("[1, 2, 3].exists_one(x, x == 2) == true"),
        },
        FunctionDef {
            name: "filter",
            signature: "(list, iter_var, predicate) -> list",
            description: "Macro that returns a new list containing only elements that satisfy the predicate.",
            example: Some("[1, 2, 3, 4].filter(x, x % 2 == 0) == [2, 4]"),
        },
        FunctionDef {
            name: "map",
            signature: "(list, iter_var, transform) -> list",
            description: "Macro that returns a new list with each element transformed by the given expression.",
            example: Some("[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
        },

        // ==================== String Functions ====================
        FunctionDef {
            name: "contains",
            signature: "(string, substring) -> bool",
            description: "Returns true if the string contains the substring.",
            example: Some("\"hello world\".contains(\"world\") == true"),
        },
        FunctionDef {
            name: "endsWith",
            signature: "(string, suffix) -> bool",
            description: "Returns true if the string ends with the given suffix.",
            example: Some("\"hello.txt\".endsWith(\".txt\") == true"),
        },
        FunctionDef {
            name: "matches",
            signature: "(string, regex) -> bool",
            description: "Returns true if the string matches the RE2 regular expression. Matches any substring by default; use ^ and $ anchors for full string match.",
            example: Some("\"hello123\".matches(\"[a-z]+[0-9]+\") == true"),
        },
        FunctionDef {
            name: "startsWith",
            signature: "(string, prefix) -> bool",
            description: "Returns true if the string starts with the given prefix.",
            example: Some("\"hello world\".startsWith(\"hello\") == true"),
        },

        // ==================== Timestamp/Duration Accessors ====================
        FunctionDef {
            name: "getDate",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of month from a timestamp (1-31). Optional timezone parameter.",
            example: Some("timestamp.getDate()"),
        },
        FunctionDef {
            name: "getDayOfMonth",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of month from a timestamp (0-30, zero-indexed). Optional timezone parameter.",
            example: Some("timestamp.getDayOfMonth()"),
        },
        FunctionDef {
            name: "getDayOfWeek",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of week from a timestamp (0-6, Sunday=0). Optional timezone parameter.",
            example: Some("timestamp.getDayOfWeek()"),
        },
        FunctionDef {
            name: "getDayOfYear",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of year from a timestamp (0-365). Optional timezone parameter.",
            example: Some("timestamp.getDayOfYear()"),
        },
        FunctionDef {
            name: "getFullYear",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the full year from a timestamp (e.g., 2023). Optional timezone parameter.",
            example: Some("timestamp.getFullYear()"),
        },
        FunctionDef {
            name: "getHours",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the hour from a timestamp (0-23) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getHours()"),
        },
        FunctionDef {
            name: "getMilliseconds",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the milliseconds from a timestamp (0-999) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getMilliseconds()"),
        },
        FunctionDef {
            name: "getMinutes",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the minutes from a timestamp (0-59) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getMinutes()"),
        },
        FunctionDef {
            name: "getMonth",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the month from a timestamp (0-11, zero-indexed). Optional timezone parameter.",
            example: Some("timestamp.getMonth()"),
        },
        FunctionDef {
            name: "getSeconds",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the seconds from a timestamp (0-59) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getSeconds()"),
        },
    ];

    defs.into_iter().map(|f| (f.name, f)).collect()
});

/// Check if a name is a CEL built-in function.
pub fn is_builtin(name: &str) -> bool {
    BUILTINS.contains_key(name)
}

/// Get documentation for a built-in function by name.
pub fn get_builtin(name: &str) -> Option<&'static FunctionDef> {
    BUILTINS.get(name)
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
        for (_, builtin) in BUILTINS.iter() {
            assert!(!builtin.name.is_empty());
            assert!(!builtin.signature.is_empty());
            assert!(!builtin.description.is_empty());
        }
    }
}
