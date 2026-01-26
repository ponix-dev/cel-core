//! Protovalidate CEL extension functions.
//!
//! This module provides CEL functions specific to protovalidate, the protobuf
//! validation library from Buf. These functions are available in CEL expressions
//! within protovalidate annotations.
//!
//! See: https://buf.build/docs/protovalidate/

use std::collections::HashMap;
use std::sync::LazyLock;

use cel_core::CelType;

use crate::types::{Arity, FunctionDef, FunctionKind};

/// Protovalidate-specific extension functions, lazily initialized.
pub static PROTOVALIDATE_BUILTINS: LazyLock<HashMap<&'static str, FunctionDef>> = LazyLock::new(|| {
    let defs = vec![
        // ==================== String Validation Methods ====================
        FunctionDef {
            name: "isEmail",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid email address according to RFC 5322.",
            example: Some("this.isEmail()"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(0)),
        },
        FunctionDef {
            name: "isHostname",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid hostname according to RFC 1123.",
            example: Some("this.isHostname()"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(0)),
        },
        FunctionDef {
            name: "isIp",
            signature: "(string, version?) -> bool",
            description: "Returns true if the string is a valid IP address. Optional version parameter: 4 for IPv4, 6 for IPv6.",
            example: Some("this.isIp() || this.isIp(4)"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
        FunctionDef {
            name: "isIpPrefix",
            signature: "(string, version?, strict?) -> bool",
            description: "Returns true if the string is a valid IP prefix (CIDR notation). Optional version (4 or 6) and strict mode parameters.",
            example: Some("this.isIpPrefix()"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 2)),
        },
        FunctionDef {
            name: "isUri",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid URI according to RFC 3986.",
            example: Some("this.isUri()"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(0)),
        },
        FunctionDef {
            name: "isUriRef",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid URI reference (can be relative).",
            example: Some("this.isUriRef()"),
            kind: FunctionKind::Method(vec![CelType::String]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(0)),
        },

        // ==================== List Methods ====================
        FunctionDef {
            name: "unique",
            signature: "(list) -> bool",
            description: "Returns true if all elements in the list are unique.",
            example: Some("this.unique()"),
            kind: FunctionKind::Method(vec![CelType::list(CelType::Dyn)]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(0)),
        },

        // ==================== Numeric Methods ====================
        FunctionDef {
            name: "isNan",
            signature: "(double) -> bool",
            description: "Returns true if the double value is NaN (Not a Number).",
            example: Some("this.isNan()"),
            kind: FunctionKind::Method(vec![CelType::Double]),
            standalone_arity: None,
            method_arity: Some(Arity::Exact(0)),
        },
        FunctionDef {
            name: "isInf",
            signature: "(double, sign?) -> bool",
            description: "Returns true if the double value is infinite. Optional sign: 1 for +Inf, -1 for -Inf, 0 for either.",
            example: Some("this.isInf() || this.isInf(1)"),
            kind: FunctionKind::Method(vec![CelType::Double]),
            standalone_arity: None,
            method_arity: Some(Arity::Range(0, 1)),
        },
    ];

    defs.into_iter().map(|f| (f.name, f)).collect()
});

/// Check if a name is a protovalidate extension function.
pub fn is_protovalidate_builtin(name: &str) -> bool {
    PROTOVALIDATE_BUILTINS.contains_key(name)
}

/// Get documentation for a protovalidate function by name.
pub fn get_protovalidate_builtin(name: &str) -> Option<&'static FunctionDef> {
    PROTOVALIDATE_BUILTINS.get(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognizes_protovalidate_builtins() {
        assert!(is_protovalidate_builtin("isEmail"));
        assert!(is_protovalidate_builtin("isUri"));
        assert!(is_protovalidate_builtin("unique"));
        assert!(is_protovalidate_builtin("isNan"));
    }

    #[test]
    fn rejects_non_protovalidate_builtins() {
        assert!(!is_protovalidate_builtin("size"));
        assert!(!is_protovalidate_builtin("has"));
        assert!(!is_protovalidate_builtin("foo"));
    }

    #[test]
    fn get_protovalidate_builtin_returns_docs() {
        let is_email = get_protovalidate_builtin("isEmail").unwrap();
        assert_eq!(is_email.name, "isEmail");
        assert!(is_email.description.contains("email"));
    }

    #[test]
    fn all_protovalidate_builtins_are_methods() {
        for (_, builtin) in PROTOVALIDATE_BUILTINS.iter() {
            assert!(
                matches!(builtin.kind, FunctionKind::Method(_)),
                "{} should be a Method",
                builtin.name
            );
            assert!(
                builtin.method_arity.is_some(),
                "{} should have method_arity",
                builtin.name
            );
        }
    }

    #[test]
    fn string_methods_accept_string() {
        let string_methods = ["isEmail", "isHostname", "isUri", "isUriRef", "isIp", "isIpPrefix"];
        for name in string_methods {
            let builtin = get_protovalidate_builtin(name).unwrap();
            match &builtin.kind {
                FunctionKind::Method(types) => {
                    assert!(
                        types.contains(&CelType::String),
                        "{} should accept String",
                        name
                    );
                }
                _ => panic!("{} should be a Method", name),
            }
        }
    }

    #[test]
    fn unique_accepts_list() {
        let unique = get_protovalidate_builtin("unique").unwrap();
        match &unique.kind {
            FunctionKind::Method(types) => {
                assert!(types.iter().any(|t| matches!(t, CelType::List(_))));
            }
            _ => panic!("unique should be a Method"),
        }
    }

    #[test]
    fn numeric_methods_accept_double() {
        let numeric_methods = ["isNan", "isInf"];
        for name in numeric_methods {
            let builtin = get_protovalidate_builtin(name).unwrap();
            match &builtin.kind {
                FunctionKind::Method(types) => {
                    assert!(
                        types.contains(&CelType::Double),
                        "{} should accept Double",
                        name
                    );
                }
                _ => panic!("{} should be a Method", name),
            }
        }
    }
}
