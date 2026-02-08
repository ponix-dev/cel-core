//! Proto file parser for extracting CEL expressions from protovalidate annotations.
//!
//! This module provides regex-based extraction of CEL expressions from .proto files
//! that use protovalidate annotations like `(buf.validate.field).cel` and
//! `(buf.validate.message).cel`.

use regex::Regex;
use std::sync::LazyLock;

use cel_core::CelType;

use crate::document::{CelRegion, OffsetMapper};

/// The context type for a protovalidate CEL expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProtovalidateContext {
    /// Field-level validation: this is the field value.
    Field {
        /// The message type containing the field (e.g., "my.api.User").
        message_type: Option<String>,
        /// The field name being validated.
        field_name: Option<String>,
        /// The proto field type (for type resolution).
        field_type: Option<String>,
    },
    /// Message-level validation: this is the message.
    Message {
        /// The message type being validated.
        message_type: Option<String>,
    },
    /// Predefined constraint: context depends on how it's used.
    Predefined,
}

impl ProtovalidateContext {
    /// Get the CEL type for `this` based on the context.
    ///
    /// Returns the appropriate type if we can determine it, otherwise Dyn.
    pub fn this_type(&self) -> CelType {
        match self {
            ProtovalidateContext::Field {
                message_type,
                field_name: _,
                field_type,
            } => {
                // If we have a field type, try to use it
                if let Some(ft) = field_type {
                    proto_field_type_to_cel(ft)
                } else if let Some(msg) = message_type {
                    // Fall back to message type if we know it
                    CelType::message(msg)
                } else {
                    CelType::Dyn
                }
            }
            ProtovalidateContext::Message { message_type } => {
                if let Some(msg) = message_type {
                    CelType::message(msg)
                } else {
                    CelType::Dyn
                }
            }
            ProtovalidateContext::Predefined => CelType::Dyn,
        }
    }
}

/// Convert a proto field type string to a CEL type.
pub(crate) fn proto_field_type_to_cel(proto_type: &str) -> CelType {
    match proto_type {
        "bool" => CelType::Bool,
        "int32" | "int64" | "sint32" | "sint64" | "sfixed32" | "sfixed64" => CelType::Int,
        "uint32" | "uint64" | "fixed32" | "fixed64" => CelType::UInt,
        "float" | "double" => CelType::Double,
        "string" => CelType::String,
        "bytes" => CelType::Bytes,
        // For message types and unknown types, use Dyn or message type
        other => {
            if let Some(inner) = other.strip_prefix("repeated ") {
                CelType::list(proto_field_type_to_cel(inner))
            } else if other.starts_with("map<") {
                // Map types are complex, fall back to dyn for now
                CelType::map(CelType::Dyn, CelType::Dyn)
            } else if other.contains('.')
                || other
                    .chars()
                    .next()
                    .map(|c| c.is_uppercase())
                    .unwrap_or(false)
            {
                // Likely a message type
                CelType::message(other)
            } else {
                CelType::Dyn
            }
        }
    }
}

/// Result of extracting a CEL expression from a proto file.
#[derive(Debug, Clone)]
pub struct ExtractedRegion {
    /// The extracted CEL source code (escape sequences decoded).
    pub source: String,

    /// Byte offset in the host document where CEL content starts (after opening quote).
    pub host_offset: usize,

    /// Escape adjustments for position mapping.
    /// Each entry is (cel_offset, cumulative_host_adjustment).
    pub escape_adjustments: Vec<(usize, usize)>,

    /// The context for this CEL expression (field, message, or predefined).
    pub context: ProtovalidateContext,
}

impl ExtractedRegion {
    /// Convert to CelRegion and OffsetMapper.
    pub fn into_region_and_mapper(self) -> (CelRegion, OffsetMapper) {
        let region = CelRegion {
            source: self.source,
        };
        let mapper = OffsetMapper::new(self.host_offset, self.escape_adjustments);
        (region, mapper)
    }
}

/// Patterns for finding protovalidate CEL option blocks, with context type.
static PROTOVALIDATE_PATTERNS: LazyLock<Vec<(Regex, ContextType)>> = LazyLock::new(|| {
    vec![
        // Field-level CEL: (buf.validate.field).cel = { ... }
        (
            Regex::new(r#"\(\s*buf\.validate\.field\s*\)\s*\.cel\s*=\s*\{"#).unwrap(),
            ContextType::Field,
        ),
        // Message-level CEL: option (buf.validate.message).cel = { ... }
        (
            Regex::new(r#"\(\s*buf\.validate\.message\s*\)\s*\.cel\s*=\s*\{"#).unwrap(),
            ContextType::Message,
        ),
        // Predefined CEL: (buf.validate.predefined).cel = { ... }
        (
            Regex::new(r#"\(\s*buf\.validate\.predefined\s*\)\s*\.cel\s*=\s*\{"#).unwrap(),
            ContextType::Predefined,
        ),
    ]
});

/// Internal context type for pattern matching.
#[derive(Debug, Clone, Copy)]
enum ContextType {
    Field,
    Message,
    Predefined,
}

/// Pattern for finding expression field within a CEL option block.
/// Note: Pattern does NOT include the opening quote - we find it separately.
static EXPRESSION_PATTERN: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"expression\s*:\s*"#).unwrap());

/// Extract all CEL regions from a proto file.
pub fn extract_cel_regions(source: &str) -> Vec<ExtractedRegion> {
    let mut regions = Vec::new();
    let comment_ranges = find_comment_ranges(source);

    for (pattern, context_type) in PROTOVALIDATE_PATTERNS.iter() {
        for mat in pattern.find_iter(source) {
            // Skip matches that fall within a comment
            if is_in_comment(mat.start(), &comment_ranges) {
                continue;
            }
            // Find the closing brace for this option block
            if let Some(block_end) = find_matching_brace(source, mat.end() - 1) {
                let block = &source[mat.end()..block_end];

                // Look for expression field within this block
                if let Some(expr_match) = EXPRESSION_PATTERN.find(block) {
                    let expr_start_in_block = expr_match.end();
                    let expr_start_in_source = mat.end() + expr_start_in_block;

                    // Determine context based on pattern type
                    let context = match context_type {
                        ContextType::Field => {
                            // Try to extract field and message info from surrounding context
                            let (message_type, field_name, field_type) =
                                extract_field_context(source, mat.start());
                            ProtovalidateContext::Field {
                                message_type,
                                field_name,
                                field_type,
                            }
                        }
                        ContextType::Message => {
                            let message_type = extract_message_context(source, mat.start());
                            ProtovalidateContext::Message { message_type }
                        }
                        ContextType::Predefined => ProtovalidateContext::Predefined,
                    };

                    // Extract the string literal
                    if let Some(extracted) =
                        extract_string_literal(source, expr_start_in_source, context)
                    {
                        regions.push(extracted);
                    }
                }
            }
        }
    }

    regions
}

/// Try to extract the field context (message type, field name, field type) from surrounding proto.
fn extract_field_context(
    source: &str,
    annotation_start: usize,
) -> (Option<String>, Option<String>, Option<String>) {
    // Look backwards for field definition: "type name = number"
    // This is a simplified heuristic - a full parser would be more accurate.
    let before = &source[..annotation_start];

    // Find the start of the line containing the field
    let line_start = before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    let line = &source[line_start..annotation_start];

    // Try to match a field pattern: type name = number
    static FIELD_PATTERN: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^\s*(?:repeated\s+)?(\w[\w.]*)\s+(\w+)\s*=\s*\d+").unwrap());

    let (field_type, field_name) = if let Some(caps) = FIELD_PATTERN.captures(line.trim()) {
        (
            caps.get(1).map(|m| m.as_str().to_string()),
            caps.get(2).map(|m| m.as_str().to_string()),
        )
    } else {
        (None, None)
    };

    // Try to find the enclosing message
    let message_type = extract_message_context(source, annotation_start);

    (message_type, field_name, field_type)
}

/// Try to extract the enclosing message type from surrounding proto.
fn extract_message_context(source: &str, position: usize) -> Option<String> {
    // Look backwards for "message Name {"
    // This is a simplified heuristic.
    let before = &source[..position];

    static MESSAGE_PATTERN: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"message\s+(\w+)\s*\{").unwrap());

    // Find all message declarations before this position and track nesting
    let mut messages: Vec<(usize, String)> = Vec::new();
    for caps in MESSAGE_PATTERN.captures_iter(before) {
        if let (Some(full), Some(name)) = (caps.get(0), caps.get(1)) {
            messages.push((full.start(), name.as_str().to_string()));
        }
    }

    // Return the last (innermost) message
    messages.pop().map(|(_, name)| name)
}

/// Find byte ranges of all comments in a proto source file.
///
/// Handles both `//` line comments and `/* */` block comments,
/// while ignoring comment-like sequences inside string literals.
fn find_comment_ranges(source: &str) -> Vec<std::ops::Range<usize>> {
    let bytes = source.as_bytes();
    let mut ranges = Vec::new();
    let mut pos = 0;
    let mut in_string = false;
    let mut escape_next = false;

    while pos < bytes.len() {
        let c = bytes[pos];

        if escape_next {
            escape_next = false;
            pos += 1;
            continue;
        }

        if in_string {
            if c == b'\\' {
                escape_next = true;
            } else if c == b'"' {
                in_string = false;
            }
            pos += 1;
            continue;
        }

        if c == b'"' {
            in_string = true;
            pos += 1;
            continue;
        }

        if c == b'/' && pos + 1 < bytes.len() {
            if bytes[pos + 1] == b'/' {
                // Line comment: extends to end of line
                let start = pos;
                pos += 2;
                while pos < bytes.len() && bytes[pos] != b'\n' {
                    pos += 1;
                }
                ranges.push(start..pos);
                continue;
            } else if bytes[pos + 1] == b'*' {
                // Block comment: extends to closing */
                let start = pos;
                pos += 2;
                while pos + 1 < bytes.len() {
                    if bytes[pos] == b'*' && bytes[pos + 1] == b'/' {
                        pos += 2;
                        break;
                    }
                    pos += 1;
                }
                ranges.push(start..pos);
                continue;
            }
        }

        pos += 1;
    }

    ranges
}

/// Check whether a byte offset falls within any comment range.
fn is_in_comment(offset: usize, comment_ranges: &[std::ops::Range<usize>]) -> bool {
    comment_ranges.iter().any(|range| range.contains(&offset))
}

/// Find the position of the matching closing brace.
fn find_matching_brace(source: &str, open_pos: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    if open_pos >= bytes.len() || bytes[open_pos] != b'{' {
        return None;
    }

    let mut depth = 1;
    let mut pos = open_pos + 1;
    let mut in_string = false;
    let mut escape_next = false;

    while pos < bytes.len() && depth > 0 {
        let c = bytes[pos];

        if escape_next {
            escape_next = false;
            pos += 1;
            continue;
        }

        if c == b'\\' && in_string {
            escape_next = true;
            pos += 1;
            continue;
        }

        if c == b'"' {
            in_string = !in_string;
        } else if !in_string {
            if c == b'{' {
                depth += 1;
            } else if c == b'}' {
                depth -= 1;
            }
        }

        pos += 1;
    }

    if depth == 0 {
        Some(pos - 1)
    } else {
        None
    }
}

/// Extract a proto string literal starting at the given position.
/// Returns the decoded content with escape sequence mappings.
fn extract_string_literal(
    source: &str,
    start: usize,
    context: ProtovalidateContext,
) -> Option<ExtractedRegion> {
    let bytes = source.as_bytes();
    if start >= bytes.len() {
        return None;
    }

    // Verify we're at a quote
    if bytes[start] != b'"' {
        return None;
    }

    let content_start = start + 1; // After the opening quote
    let mut pos = content_start;
    let mut content = String::new();
    let mut escape_adjustments = Vec::new();
    let mut cumulative_adjustment = 0;

    while pos < bytes.len() {
        let c = bytes[pos];

        if c == b'"' {
            // End of string
            return Some(ExtractedRegion {
                source: content,
                host_offset: content_start,
                escape_adjustments,
                context,
            });
        } else if c == b'\\' && pos + 1 < bytes.len() {
            // Handle escape sequence
            let escaped = bytes[pos + 1];
            let char_to_add = match escaped {
                b'n' => '\n',
                b't' => '\t',
                b'r' => '\r',
                b'\\' => '\\',
                b'"' => '"',
                b'\'' => '\'',
                b'0' => '\0',
                // For unknown escapes, just use the escaped char
                _ => escaped as char,
            };

            content.push(char_to_add);
            cumulative_adjustment += 1; // We consumed 2 bytes but added 1 char

            // Record the adjustment at this CEL offset
            escape_adjustments.push((content.len(), cumulative_adjustment));

            pos += 2;
        } else {
            content.push(c as char);
            pos += 1;
        }
    }

    // Unclosed string
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_field_level_cel() {
        let proto = r#"
message User {
    string email = 1 [(buf.validate.field).cel = {
        expression: "this.isEmail()"
    }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);
        assert_eq!(regions[0].source, "this.isEmail()");
        // Should be field context
        assert!(matches!(
            regions[0].context,
            ProtovalidateContext::Field { .. }
        ));
    }

    #[test]
    fn extracts_message_level_cel() {
        let proto = r#"
message User {
    string first_name = 1;
    string last_name = 2;

    option (buf.validate.message).cel = {
        id: "name_check"
        expression: "!has(this.first_name) || has(this.last_name)"
    };
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);
        assert_eq!(
            regions[0].source,
            "!has(this.first_name) || has(this.last_name)"
        );
        // Should be message context with User
        assert!(matches!(
            &regions[0].context,
            ProtovalidateContext::Message { message_type: Some(name) } if name == "User"
        ));
    }

    #[test]
    fn extracts_multiple_regions() {
        let proto = r#"
message User {
    string email = 1 [(buf.validate.field).cel = {
        expression: "this.isEmail()"
    }];
    string name = 2 [(buf.validate.field).cel = {
        expression: "size(this) > 0"
    }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 2);
        assert_eq!(regions[0].source, "this.isEmail()");
        assert_eq!(regions[1].source, "size(this) > 0");
    }

    #[test]
    fn handles_escaped_quotes() {
        let proto = r#"
message Test {
    string val = 1 [(buf.validate.field).cel = {
        expression: "this.contains(\"hello\")"
    }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);
        assert_eq!(regions[0].source, r#"this.contains("hello")"#);

        // Verify escape adjustments are recorded
        assert!(!regions[0].escape_adjustments.is_empty());
    }

    #[test]
    fn handles_other_escapes() {
        let proto = r#"
message Test {
    string val = 1 [(buf.validate.field).cel = {
        expression: "line1\nline2"
    }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);
        assert_eq!(regions[0].source, "line1\nline2");
    }

    #[test]
    fn no_regions_in_plain_proto() {
        let proto = r#"
message User {
    string email = 1;
    string name = 2;
}
"#;
        let regions = extract_cel_regions(proto);
        assert!(regions.is_empty());
    }

    #[test]
    fn correct_host_offset() {
        let proto = r#"[(buf.validate.field).cel = { expression: "test" }]"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);

        // Verify the host_offset points to the start of "test" content
        let offset = regions[0].host_offset;
        assert_eq!(&proto[offset..offset + 4], "test");
    }

    #[test]
    fn find_matching_brace_simple() {
        let s = "{ foo }";
        assert_eq!(find_matching_brace(s, 0), Some(6));
    }

    #[test]
    fn find_matching_brace_nested() {
        let s = "{ foo { bar } }";
        assert_eq!(find_matching_brace(s, 0), Some(14));
    }

    #[test]
    fn find_matching_brace_with_string() {
        let s = r#"{ foo "}" bar }"#;
        assert_eq!(find_matching_brace(s, 0), Some(14));
    }

    #[test]
    fn extracts_field_context() {
        let proto = r#"
message User {
    string email = 1 [(buf.validate.field).cel = {
        expression: "this.isEmail()"
    }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);

        match &regions[0].context {
            ProtovalidateContext::Field {
                message_type,
                field_name,
                field_type,
            } => {
                assert_eq!(message_type.as_deref(), Some("User"));
                assert_eq!(field_name.as_deref(), Some("email"));
                assert_eq!(field_type.as_deref(), Some("string"));
            }
            _ => panic!("Expected Field context"),
        }
    }

    #[test]
    fn context_this_type_for_string_field() {
        let context = ProtovalidateContext::Field {
            message_type: Some("User".to_string()),
            field_name: Some("email".to_string()),
            field_type: Some("string".to_string()),
        };
        assert_eq!(context.this_type(), CelType::String);
    }

    #[test]
    fn context_this_type_for_message() {
        let context = ProtovalidateContext::Message {
            message_type: Some("User".to_string()),
        };
        assert_eq!(context.this_type(), CelType::message("User"));
    }

    #[test]
    fn context_this_type_for_predefined() {
        let context = ProtovalidateContext::Predefined;
        assert_eq!(context.this_type(), CelType::Dyn);
    }

    #[test]
    fn skips_line_commented_annotation() {
        let proto = r#"
message User {
    // string email = 1 [(buf.validate.field).cel = {
    //     expression: "this.isEmail()"
    // }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert!(regions.is_empty());
    }

    #[test]
    fn skips_block_commented_annotation() {
        let proto = r#"
message User {
    /* string email = 1 [(buf.validate.field).cel = {
        expression: "this.isEmail()"
    }]; */
}
"#;
        let regions = extract_cel_regions(proto);
        assert!(regions.is_empty());
    }

    #[test]
    fn extracts_uncommented_but_skips_commented() {
        let proto = r#"
message User {
    string email = 1 [(buf.validate.field).cel = {
        expression: "this.isEmail()"
    }];
    // string name = 2 [(buf.validate.field).cel = {
    //     expression: "size(this) > 0"
    // }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);
        assert_eq!(regions[0].source, "this.isEmail()");
    }

    #[test]
    fn comment_like_content_in_strings_not_treated_as_comment() {
        // A string containing "//" should not confuse the comment parser
        let proto = r#"
message Test {
    string val = 1 [(buf.validate.field).cel = {
        expression: "this.contains(\"//\")"
    }];
}
"#;
        let regions = extract_cel_regions(proto);
        assert_eq!(regions.len(), 1);
        assert_eq!(regions[0].source, r#"this.contains("//")"#);
    }
}
