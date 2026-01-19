//! Proto file parser for extracting CEL expressions from protovalidate annotations.
//!
//! This module provides regex-based extraction of CEL expressions from .proto files
//! that use protovalidate annotations like `(buf.validate.field).cel` and
//! `(buf.validate.message).cel`.

use regex::Regex;
use std::sync::LazyLock;

use crate::document::{CelRegion, OffsetMapper};

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

/// Patterns for finding protovalidate CEL option blocks.
static PROTOVALIDATE_PATTERNS: LazyLock<Vec<Regex>> = LazyLock::new(|| {
    vec![
        // Field-level CEL: (buf.validate.field).cel = { ... }
        Regex::new(r#"\(\s*buf\.validate\.field\s*\)\s*\.cel\s*=\s*\{"#).unwrap(),
        // Message-level CEL: option (buf.validate.message).cel = { ... }
        Regex::new(r#"\(\s*buf\.validate\.message\s*\)\s*\.cel\s*=\s*\{"#).unwrap(),
        // Predefined CEL: (buf.validate.predefined).cel = { ... }
        Regex::new(r#"\(\s*buf\.validate\.predefined\s*\)\s*\.cel\s*=\s*\{"#).unwrap(),
    ]
});

/// Pattern for finding expression field within a CEL option block.
/// Note: Pattern does NOT include the opening quote - we find it separately.
static EXPRESSION_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"expression\s*:\s*"#).unwrap()
});

/// Extract all CEL regions from a proto file.
pub fn extract_cel_regions(source: &str) -> Vec<ExtractedRegion> {
    let mut regions = Vec::new();

    for pattern in PROTOVALIDATE_PATTERNS.iter() {
        for mat in pattern.find_iter(source) {
            // Find the closing brace for this option block
            if let Some(block_end) = find_matching_brace(source, mat.end() - 1) {
                let block = &source[mat.end()..block_end];

                // Look for expression field within this block
                if let Some(expr_match) = EXPRESSION_PATTERN.find(block) {
                    let expr_start_in_block = expr_match.end();
                    let expr_start_in_source = mat.end() + expr_start_in_block;

                    // Extract the string literal
                    if let Some(extracted) = extract_string_literal(source, expr_start_in_source) {
                        regions.push(extracted);
                    }
                }
            }
        }
    }

    regions
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
fn extract_string_literal(source: &str, start: usize) -> Option<ExtractedRegion> {
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
}
