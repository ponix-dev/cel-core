//! Protovalidate CEL extension functions.
//!
//! This module provides CEL functions specific to protovalidate, the protobuf
//! validation library from Buf. These functions are available in CEL expressions
//! within protovalidate annotations.
//!
//! See: https://buf.build/docs/protovalidate/

use std::collections::HashMap;
use std::sync::LazyLock;

use crate::types::FunctionDef;

/// Protovalidate-specific extension functions, lazily initialized.
pub static PROTOVALIDATE_BUILTINS: LazyLock<HashMap<&'static str, FunctionDef>> = LazyLock::new(
    || {
        let defs = vec![
        // ==================== String Validation Methods ====================
        FunctionDef {
            name: "isEmail",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid email address according to RFC 5322.",
            example: Some("this.isEmail()"),
        },
        FunctionDef {
            name: "isHostname",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid hostname according to RFC 1123.",
            example: Some("this.isHostname()"),
        },
        FunctionDef {
            name: "isIp",
            signature: "(string, version?) -> bool",
            description: "Returns true if the string is a valid IP address. Optional version parameter: 4 for IPv4, 6 for IPv6.",
            example: Some("this.isIp() || this.isIp(4)"),
        },
        FunctionDef {
            name: "isIpPrefix",
            signature: "(string, version?, strict?) -> bool",
            description: "Returns true if the string is a valid IP prefix (CIDR notation). Optional version (4 or 6) and strict mode parameters.",
            example: Some("this.isIpPrefix()"),
        },
        FunctionDef {
            name: "isUri",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid URI according to RFC 3986.",
            example: Some("this.isUri()"),
        },
        FunctionDef {
            name: "isUriRef",
            signature: "(string) -> bool",
            description: "Returns true if the string is a valid URI reference (can be relative).",
            example: Some("this.isUriRef()"),
        },

        // ==================== List Methods ====================
        FunctionDef {
            name: "unique",
            signature: "(list) -> bool",
            description: "Returns true if all elements in the list are unique.",
            example: Some("this.unique()"),
        },

        // ==================== Numeric Methods ====================
        FunctionDef {
            name: "isNan",
            signature: "(double) -> bool",
            description: "Returns true if the double value is NaN (Not a Number).",
            example: Some("this.isNan()"),
        },
        FunctionDef {
            name: "isInf",
            signature: "(double, sign?) -> bool",
            description: "Returns true if the double value is infinite. Optional sign: 1 for +Inf, -1 for -Inf, 0 for either.",
            example: Some("this.isInf() || this.isInf(1)"),
        },
    ];

        defs.into_iter().map(|f| (f.name, f)).collect()
    },
);

/// Get documentation for a protovalidate function by name.
pub fn get_protovalidate_builtin(name: &str) -> Option<&'static FunctionDef> {
    PROTOVALIDATE_BUILTINS.get(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognizes_protovalidate_builtins() {
        assert!(PROTOVALIDATE_BUILTINS.contains_key("isEmail"));
        assert!(PROTOVALIDATE_BUILTINS.contains_key("isUri"));
        assert!(PROTOVALIDATE_BUILTINS.contains_key("unique"));
        assert!(PROTOVALIDATE_BUILTINS.contains_key("isNan"));
    }

    #[test]
    fn rejects_non_protovalidate_builtins() {
        assert!(!PROTOVALIDATE_BUILTINS.contains_key("size"));
        assert!(!PROTOVALIDATE_BUILTINS.contains_key("has"));
        assert!(!PROTOVALIDATE_BUILTINS.contains_key("foo"));
    }

    #[test]
    fn get_protovalidate_builtin_returns_docs() {
        let is_email = get_protovalidate_builtin("isEmail").unwrap();
        assert_eq!(is_email.name, "isEmail");
        assert!(is_email.description.contains("email"));
    }

    #[test]
    fn all_protovalidate_builtins_have_docs() {
        for (_, builtin) in PROTOVALIDATE_BUILTINS.iter() {
            assert!(!builtin.name.is_empty());
            assert!(!builtin.signature.is_empty());
            assert!(!builtin.description.is_empty());
        }
    }
}
