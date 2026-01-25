//! CEL built-in functions and macros with documentation and type information.

use std::collections::HashMap;
use std::sync::LazyLock;

use cel_core_common::CelType;

use super::function::{Arity, FunctionDef, FunctionKind};

/// All CEL built-in functions with documentation, lazily initialized.
pub static BUILTINS: LazyLock<HashMap<&'static str, FunctionDef>> = LazyLock::new(|| {
    let defs = vec![
        // ==================== Type Conversions ====================
        FunctionDef {
            name: "bool",
            signature: "(value) -> bool",
            description: "Type conversion to bool. Accepts bool (identity) or string (\"true\" → true, \"false\" → false).",
            example: Some("bool(\"true\") == true"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "bytes",
            signature: "(value) -> bytes",
            description: "Type conversion to bytes. Accepts bytes (identity) or string (UTF-8 encoded).",
            example: Some("bytes(\"hello\")"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "double",
            signature: "(value) -> double",
            description: "Type conversion to double. Accepts int, uint, string (parsed as float), or double.",
            example: Some("double(\"3.14\") == 3.14"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "duration",
            signature: "(value) -> google.protobuf.Duration",
            description: "Type conversion to duration. Accepts duration (identity) or string (format: sequence of decimal numbers with time unit suffix h, m, s, ms, us, ns).",
            example: Some("duration(\"1h30m\")"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "dyn",
            signature: "(value) -> dyn",
            description: "Converts a value to the dyn type, enabling dynamic dispatch. Useful for heterogeneous collections.",
            example: Some("dyn(x)"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "int",
            signature: "(value) -> int",
            description: "Type conversion to int (64-bit signed). Accepts int (identity), uint, double (truncates toward zero), string (parses), or timestamp (Unix seconds).",
            example: Some("int(\"42\") == 42"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "string",
            signature: "(value) -> string",
            description: "Type conversion to string. Works with all primitive types, timestamps, durations, bytes (UTF-8 decode).",
            example: Some("string(123) == \"123\""),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "timestamp",
            signature: "(value) -> google.protobuf.Timestamp",
            description: "Type conversion to timestamp. Accepts timestamp (identity) or string (RFC 3339 format).",
            example: Some("timestamp(\"2023-01-15T10:30:00Z\")"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "type",
            signature: "(value) -> type",
            description: "Returns the runtime type of a value as a type value.",
            example: Some("type(1) == int"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },
        FunctionDef {
            name: "uint",
            signature: "(value) -> uint",
            description: "Type conversion to uint (64-bit unsigned). Accepts uint (identity), int, double (truncates toward zero), or string (parses).",
            example: Some("uint(\"42\") == 42u"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },

        // ==================== Size & Existence ====================
        FunctionDef {
            name: "size",
            signature: "(string|bytes|list|map) -> int",
            description: "Returns the length of a string (in Unicode code points), bytes (byte length), list (element count), or map (entry count).",
            example: Some("size(\"hello\") == 5"),
            kind: FunctionKind::Both(vec![
                CelType::String,
                CelType::Bytes,
                CelType::list(CelType::Dyn),
                CelType::map(CelType::Dyn, CelType::Dyn),
            ]),
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: Some(Arity::Exact(0)),
        },
        FunctionDef {
            name: "has",
            signature: "(field) -> bool",
            description: "Macro that tests whether a field is present. Returns true if the field exists and is set, false otherwise. Does not evaluate the field.",
            example: Some("has(msg.optional_field)"),
            kind: FunctionKind::Standalone,
            standalone_arity: Some(Arity::Exact(1)),
            method_arity: None,
        },

        // ==================== Macros (Comprehensions) ====================
        FunctionDef {
            name: "all",
            signature: "(list, iter_var, predicate) -> bool",
            description: "Macro that tests whether all elements in a list satisfy the predicate. Returns true for empty lists. Short-circuits on first false.",
            example: Some("[1, 2, 3].all(x, x > 0) == true"),
            kind: FunctionKind::Method(vec![
                CelType::list(CelType::Dyn),
                CelType::map(CelType::Dyn, CelType::Dyn),
            ]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(2)),
        },
        FunctionDef {
            name: "exists",
            signature: "(list, iter_var, predicate) -> bool",
            description: "Macro that tests whether any element in a list satisfies the predicate. Returns false for empty lists. Short-circuits on first true.",
            example: Some("[1, 2, 3].exists(x, x == 2) == true"),
            kind: FunctionKind::Method(vec![
                CelType::list(CelType::Dyn),
                CelType::map(CelType::Dyn, CelType::Dyn),
            ]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(2)),
        },
        FunctionDef {
            name: "exists_one",
            signature: "(list, iter_var, predicate) -> bool",
            description: "Macro that tests whether exactly one element in a list satisfies the predicate. Does not short-circuit.",
            example: Some("[1, 2, 3].exists_one(x, x == 2) == true"),
            kind: FunctionKind::Method(vec![
                CelType::list(CelType::Dyn),
                CelType::map(CelType::Dyn, CelType::Dyn),
            ]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(2)),
        },
        FunctionDef {
            name: "filter",
            signature: "(list, iter_var, predicate) -> list",
            description: "Macro that returns a new list containing only elements that satisfy the predicate.",
            example: Some("[1, 2, 3, 4].filter(x, x % 2 == 0) == [2, 4]"),
            kind: FunctionKind::Method(vec![
                CelType::list(CelType::Dyn),
                CelType::map(CelType::Dyn, CelType::Dyn),
            ]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(2)),
        },
        FunctionDef {
            name: "map",
            signature: "(list, iter_var, transform) -> list",
            description: "Macro that returns a new list with each element transformed by the given expression.",
            example: Some("[1, 2, 3].map(x, x * 2) == [2, 4, 6]"),
            kind: FunctionKind::Method(vec![
                CelType::list(CelType::Dyn),
                CelType::map(CelType::Dyn, CelType::Dyn),
            ]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(2, 3)), // Can have optional filter predicate
        },

        // ==================== String Functions ====================
        FunctionDef {
            name: "contains",
            signature: "(string, substring) -> bool",
            description: "Returns true if the string contains the substring.",
            example: Some("\"hello world\".contains(\"world\") == true"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(1)),
        },
        FunctionDef {
            name: "endsWith",
            signature: "(string, suffix) -> bool",
            description: "Returns true if the string ends with the given suffix.",
            example: Some("\"hello.txt\".endsWith(\".txt\") == true"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(1)),
        },
        FunctionDef {
            name: "matches",
            signature: "(string, regex) -> bool",
            description: "Returns true if the string matches the RE2 regular expression. Matches any substring by default; use ^ and $ anchors for full string match.",
            example: Some("\"hello123\".matches(\"[a-z]+[0-9]+\") == true"),
            kind: FunctionKind::Both(vec![CelType::String]),
            standalone_arity: Some(Arity::Exact(2)),
            method_arity: Some(Arity::Exact(1)),
        },
        FunctionDef {
            name: "startsWith",
            signature: "(string, prefix) -> bool",
            description: "Returns true if the string starts with the given prefix.",
            example: Some("\"hello world\".startsWith(\"hello\") == true"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(1)),
        },

        // ==================== Timestamp/Duration Accessors ====================
        FunctionDef {
            name: "getDate",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of month from a timestamp (1-31). Optional timezone parameter.",
            example: Some("timestamp.getDate()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getDayOfMonth",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of month from a timestamp (0-30, zero-indexed). Optional timezone parameter.",
            example: Some("timestamp.getDayOfMonth()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getDayOfWeek",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of week from a timestamp (0-6, Sunday=0). Optional timezone parameter.",
            example: Some("timestamp.getDayOfWeek()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getDayOfYear",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the day of year from a timestamp (0-365). Optional timezone parameter.",
            example: Some("timestamp.getDayOfYear()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getFullYear",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the full year from a timestamp (e.g., 2023). Optional timezone parameter.",
            example: Some("timestamp.getFullYear()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getHours",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the hour from a timestamp (0-23) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getHours()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp, CelType::Duration]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getMilliseconds",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the milliseconds from a timestamp (0-999) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getMilliseconds()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp, CelType::Duration]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getMinutes",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the minutes from a timestamp (0-59) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getMinutes()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp, CelType::Duration]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getMonth",
            signature: "(timestamp, timezone?) -> int",
            description: "Returns the month from a timestamp (0-11, zero-indexed). Optional timezone parameter.",
            example: Some("timestamp.getMonth()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "getSeconds",
            signature: "(timestamp|duration, timezone?) -> int",
            description: "Returns the seconds from a timestamp (0-59) or duration. Optional timezone parameter for timestamps.",
            example: Some("timestamp.getSeconds()"),
            kind: FunctionKind::Method(vec![CelType::Timestamp, CelType::Duration]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
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
        // Note: matches is Both (standalone + method), not Method-only
        let string_methods = ["endsWith", "startsWith", "contains"];
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
                assert!(types.iter().any(|t| matches!(t, CelType::List(_))));
                assert!(types.iter().any(|t| matches!(t, CelType::Map(_, _))));
            }
            _ => panic!("size should be Both"),
        }
    }

    #[test]
    fn matches_is_both() {
        let matches = get_builtin("matches").unwrap();
        match &matches.kind {
            FunctionKind::Both(types) => {
                assert!(types.contains(&CelType::String));
                assert_eq!(types.len(), 1, "matches should only accept String");
            }
            _ => panic!("matches should be Both"),
        }
        assert_eq!(matches.standalone_arity, Some(Arity::Exact(2)));
        assert_eq!(matches.method_arity, Some(Arity::Exact(1)));
    }

    #[test]
    fn arity_exact_validation() {
        let arity = Arity::Exact(1);
        assert!(!arity.is_valid(0));
        assert!(arity.is_valid(1));
        assert!(!arity.is_valid(2));
        assert_eq!(arity.description(), "1");
    }

    #[test]
    fn arity_range_validation() {
        let arity = Arity::Range(0, 1);
        assert!(arity.is_valid(0));
        assert!(arity.is_valid(1));
        assert!(!arity.is_valid(2));
        assert_eq!(arity.description(), "0 to 1");

        // Same min/max should show single number
        let single = Arity::Range(2, 2);
        assert_eq!(single.description(), "2");
    }

    #[test]
    fn all_builtins_have_arity() {
        for (_, builtin) in BUILTINS.iter() {
            // Each builtin should have at least one arity defined
            assert!(
                builtin.standalone_arity.is_some() || builtin.method_arity.is_some(),
                "{} should have standalone_arity or method_arity defined",
                builtin.name
            );

            // Standalone functions should have standalone_arity
            if builtin.kind == FunctionKind::Standalone {
                assert!(
                    builtin.standalone_arity.is_some(),
                    "{} is Standalone but has no standalone_arity",
                    builtin.name
                );
            }

            // Method-only functions should have method_arity
            if matches!(builtin.kind, FunctionKind::Method(_)) {
                assert!(
                    builtin.method_arity.is_some(),
                    "{} is Method but has no method_arity",
                    builtin.name
                );
            }

            // Both functions should have both arities
            if matches!(builtin.kind, FunctionKind::Both(_)) {
                assert!(
                    builtin.standalone_arity.is_some() && builtin.method_arity.is_some(),
                    "{} is Both but doesn't have both arities",
                    builtin.name
                );
            }
        }
    }

    #[test]
    fn size_has_correct_arity() {
        let size = get_builtin("size").unwrap();
        assert_eq!(size.standalone_arity, Some(Arity::Exact(1)));
        assert_eq!(size.method_arity, Some(Arity::Exact(0)));
    }

    #[test]
    fn map_macro_has_variable_arity() {
        let map = get_builtin("map").unwrap();
        assert_eq!(map.method_arity, Some(Arity::Range(2, 3)));
    }

    #[test]
    fn timestamp_methods_have_optional_arg() {
        let timestamp_methods = ["getDate", "getHours", "getMinutes", "getSeconds"];
        for name in timestamp_methods {
            let builtin = get_builtin(name).unwrap();
            assert_eq!(
                builtin.method_arity,
                Some(Arity::Range(0, 1)),
                "{} should have optional timezone arg",
                name
            );
        }
    }
}
