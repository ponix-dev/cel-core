//! String extension library for CEL.
//!
//! This module provides additional string manipulation functions beyond the
//! CEL standard library, matching the cel-go strings extension.
//!
//! # Functions
//!
//! - `charAt(index)` - Returns character at index as a string
//! - `indexOf(substring)` / `indexOf(substring, offset)` - Find first occurrence
//! - `lastIndexOf(substring)` / `lastIndexOf(substring, offset)` - Find last occurrence
//! - `lowerAscii()` - Convert ASCII characters to lowercase
//! - `upperAscii()` - Convert ASCII characters to uppercase
//! - `replace(old, new)` / `replace(old, new, count)` - Replace occurrences
//! - `split(separator)` / `split(separator, limit)` - Split string into list
//! - `substring(start)` / `substring(start, end)` - Extract substring
//! - `trim()` - Remove leading/trailing whitespace
//! - `reverse()` - Reverse the string (Unicode-aware)
//! - `format(args)` - Format string with arguments
//! - `join()` / `join(separator)` - Join list of strings (method on list<string>)
//! - `strings.quote(string)` - Quote a string with escapes

use crate::types::{CelType, FunctionDecl, OverloadDecl};

/// Returns the string extension library function declarations.
pub fn string_extension() -> Vec<FunctionDecl> {
    vec![
        // charAt: (string).charAt(int) -> string
        FunctionDecl::new("charAt").with_overload(OverloadDecl::method(
            "string_char_at_int",
            vec![CelType::String, CelType::Int],
            CelType::String,
        )),
        // indexOf: two overloads
        FunctionDecl::new("indexOf")
            .with_overload(OverloadDecl::method(
                "string_index_of_string",
                vec![CelType::String, CelType::String],
                CelType::Int,
            ))
            .with_overload(OverloadDecl::method(
                "string_index_of_string_int",
                vec![CelType::String, CelType::String, CelType::Int],
                CelType::Int,
            )),
        // lastIndexOf
        FunctionDecl::new("lastIndexOf")
            .with_overload(OverloadDecl::method(
                "string_last_index_of_string",
                vec![CelType::String, CelType::String],
                CelType::Int,
            ))
            .with_overload(OverloadDecl::method(
                "string_last_index_of_string_int",
                vec![CelType::String, CelType::String, CelType::Int],
                CelType::Int,
            )),
        // lowerAscii
        FunctionDecl::new("lowerAscii").with_overload(OverloadDecl::method(
            "string_lower_ascii",
            vec![CelType::String],
            CelType::String,
        )),
        // upperAscii
        FunctionDecl::new("upperAscii").with_overload(OverloadDecl::method(
            "string_upper_ascii",
            vec![CelType::String],
            CelType::String,
        )),
        // replace
        FunctionDecl::new("replace")
            .with_overload(OverloadDecl::method(
                "string_replace_string_string",
                vec![CelType::String, CelType::String, CelType::String],
                CelType::String,
            ))
            .with_overload(OverloadDecl::method(
                "string_replace_string_string_int",
                vec![CelType::String, CelType::String, CelType::String, CelType::Int],
                CelType::String,
            )),
        // split
        FunctionDecl::new("split")
            .with_overload(OverloadDecl::method(
                "string_split_string",
                vec![CelType::String, CelType::String],
                CelType::list(CelType::String),
            ))
            .with_overload(OverloadDecl::method(
                "string_split_string_int",
                vec![CelType::String, CelType::String, CelType::Int],
                CelType::list(CelType::String),
            )),
        // substring
        FunctionDecl::new("substring")
            .with_overload(OverloadDecl::method(
                "string_substring_int",
                vec![CelType::String, CelType::Int],
                CelType::String,
            ))
            .with_overload(OverloadDecl::method(
                "string_substring_int_int",
                vec![CelType::String, CelType::Int, CelType::Int],
                CelType::String,
            )),
        // trim
        FunctionDecl::new("trim").with_overload(OverloadDecl::method(
            "string_trim",
            vec![CelType::String],
            CelType::String,
        )),
        // reverse
        FunctionDecl::new("reverse").with_overload(OverloadDecl::method(
            "string_reverse",
            vec![CelType::String],
            CelType::String,
        )),
        // format
        FunctionDecl::new("format").with_overload(OverloadDecl::method(
            "string_format",
            vec![CelType::String, CelType::list(CelType::Dyn)],
            CelType::String,
        )),
        // join - method on list<string>
        FunctionDecl::new("join")
            .with_overload(OverloadDecl::method(
                "list_string_join",
                vec![CelType::list(CelType::String)],
                CelType::String,
            ))
            .with_overload(OverloadDecl::method(
                "list_string_join_string",
                vec![CelType::list(CelType::String), CelType::String],
                CelType::String,
            )),
        // strings.quote - namespaced standalone function
        FunctionDecl::new("strings.quote").with_overload(OverloadDecl::function(
            "strings_quote_string",
            vec![CelType::String],
            CelType::String,
        )),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_extension_count() {
        let funcs = string_extension();
        // 13 functions defined
        assert_eq!(funcs.len(), 13);
    }

    #[test]
    fn test_char_at() {
        let funcs = string_extension();
        let char_at = funcs.iter().find(|f| f.name == "charAt").unwrap();
        assert_eq!(char_at.overloads.len(), 1);
        assert!(char_at.overloads[0].is_member);
    }

    #[test]
    fn test_index_of_overloads() {
        let funcs = string_extension();
        let index_of = funcs.iter().find(|f| f.name == "indexOf").unwrap();
        assert_eq!(index_of.overloads.len(), 2);
    }

    #[test]
    fn test_join_is_member_on_list() {
        let funcs = string_extension();
        let join = funcs.iter().find(|f| f.name == "join").unwrap();
        assert_eq!(join.overloads.len(), 2);
        for overload in &join.overloads {
            assert!(overload.is_member);
            assert_eq!(overload.receiver_type(), Some(&CelType::list(CelType::String)));
        }
    }

    #[test]
    fn test_strings_quote_is_standalone() {
        let funcs = string_extension();
        let quote = funcs.iter().find(|f| f.name == "strings.quote").unwrap();
        assert_eq!(quote.overloads.len(), 1);
        assert!(!quote.overloads[0].is_member);
    }
}
