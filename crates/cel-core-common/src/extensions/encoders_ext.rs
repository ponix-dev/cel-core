//! Encoders extension library for CEL.
//!
//! This module provides encoding/decoding functions for CEL,
//! matching the cel-go encoders extension.
//!
//! # Functions
//!
//! - `base64.encode(bytes) -> string` - Encodes bytes to base64 string
//! - `base64.decode(string) -> bytes` - Decodes base64 string to bytes

use crate::{CelType, FunctionDecl, OverloadDecl};

/// Returns the encoders extension library function declarations.
pub fn encoders_extension() -> Vec<FunctionDecl> {
    vec![
        // base64.encode(bytes) -> string
        FunctionDecl::new("base64.encode").with_overload(OverloadDecl::function(
            "base64_encode_bytes",
            vec![CelType::Bytes],
            CelType::String,
        )),
        // base64.decode(string) -> bytes
        FunctionDecl::new("base64.decode").with_overload(OverloadDecl::function(
            "base64_decode_string",
            vec![CelType::String],
            CelType::Bytes,
        )),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encoders_extension_has_functions() {
        let funcs = encoders_extension();
        assert_eq!(funcs.len(), 2);
    }

    #[test]
    fn test_base64_encode() {
        let funcs = encoders_extension();
        let encode = funcs.iter().find(|f| f.name == "base64.encode").unwrap();
        assert_eq!(encode.overloads.len(), 1);
        assert_eq!(encode.overloads[0].id, "base64_encode_bytes");
        assert_eq!(encode.overloads[0].params, vec![CelType::Bytes]);
        assert_eq!(encode.overloads[0].result, CelType::String);
    }

    #[test]
    fn test_base64_decode() {
        let funcs = encoders_extension();
        let decode = funcs.iter().find(|f| f.name == "base64.decode").unwrap();
        assert_eq!(decode.overloads.len(), 1);
        assert_eq!(decode.overloads[0].id, "base64_decode_string");
        assert_eq!(decode.overloads[0].params, vec![CelType::String]);
        assert_eq!(decode.overloads[0].result, CelType::Bytes);
    }

    #[test]
    fn test_all_functions_are_standalone() {
        let funcs = encoders_extension();
        for func in &funcs {
            for overload in &func.overloads {
                assert!(
                    !overload.is_member,
                    "Expected {} to be standalone, but it's a member function",
                    overload.id
                );
            }
        }
    }
}
