//! CEL region extraction and offset mapping for embedded CEL expressions.
//!
//! This module provides types for tracking CEL expressions embedded within
//! host documents (like .proto files) and mapping between CEL-local coordinates
//! and host document coordinates.

use std::ops::Range;

use cel_parser::{ParseError, SpannedExpr};

use crate::types::ValidationError;

/// Represents a single CEL expression region within a host document.
#[derive(Debug, Clone)]
pub struct CelRegion {
    /// The extracted CEL source code (without surrounding quotes).
    pub source: String,
}

/// Maps between CEL expression local coordinates and host document coordinates.
///
/// When CEL is embedded in a host document (like a .proto file), the CEL parser
/// produces spans relative to the start of the CEL string. This mapper translates
/// those spans to positions in the host document, accounting for:
/// - The offset where the CEL content starts in the host
/// - Escape sequences in the host string that compress in CEL (e.g., `\"` → `"`)
#[derive(Debug, Clone)]
pub struct OffsetMapper {
    /// Byte offset where CEL content starts in host document.
    host_offset: usize,

    /// Escape sequence adjustments: (cel_offset, host_adjustment).
    /// Each entry indicates that at the given CEL offset, the host document
    /// consumed extra bytes due to escape sequences.
    ///
    /// For example, if the host has `\"` at position 10, and CEL sees `"`,
    /// we record that at CEL offset (position after the escape in CEL),
    /// the host has consumed 1 extra byte.
    escape_adjustments: Vec<(usize, usize)>,
}

impl OffsetMapper {
    /// Create a new offset mapper.
    ///
    /// # Arguments
    /// * `host_offset` - Byte offset where CEL content starts in host document
    /// * `escape_adjustments` - List of (cel_offset, cumulative_adjustment) pairs
    pub fn new(host_offset: usize, escape_adjustments: Vec<(usize, usize)>) -> Self {
        Self {
            host_offset,
            escape_adjustments,
        }
    }

    /// Create a simple mapper with no escape adjustments.
    #[cfg(test)]
    pub fn simple(host_offset: usize) -> Self {
        Self::new(host_offset, Vec::new())
    }

    /// Convert a CEL-local byte offset to host document byte offset.
    pub fn to_host(&self, cel_offset: usize) -> usize {
        let adjustment = self.get_adjustment_at(cel_offset);
        self.host_offset + cel_offset + adjustment
    }

    /// Convert a CEL-local span to host document span.
    pub fn span_to_host(&self, span: &Range<usize>) -> Range<usize> {
        self.to_host(span.start)..self.to_host(span.end)
    }

    /// Get the cumulative adjustment at a given CEL offset.
    fn get_adjustment_at(&self, cel_offset: usize) -> usize {
        // Find the last adjustment that applies at or before this offset
        let mut adjustment = 0;
        for (threshold, adj) in &self.escape_adjustments {
            if cel_offset >= *threshold {
                adjustment = *adj;
            } else {
                break;
            }
        }
        adjustment
    }

    /// Get the host offset where the CEL region starts.
    pub fn host_offset(&self) -> usize {
        self.host_offset
    }

    /// Get the length of the CEL source in host document bytes.
    /// This is the CEL length plus all escape adjustments.
    pub fn host_length(&self, cel_length: usize) -> usize {
        cel_length + self.get_adjustment_at(cel_length)
    }
}

/// State for a single CEL region with its offset mapper and analysis results.
#[derive(Debug, Clone)]
pub struct CelRegionState {
    /// The CEL region data.
    pub region: CelRegion,

    /// Offset mapper for this region.
    pub mapper: OffsetMapper,

    /// Parsed AST for this region (may be partial with Expr::Error nodes).
    pub ast: Option<SpannedExpr>,

    /// Parse errors for this region (spans are relative to region).
    pub parse_errors: Vec<ParseError>,

    /// Validation errors for this region (spans are relative to region).
    pub validation_errors: Vec<ValidationError>,
}

impl CelRegionState {
    /// Create a new CEL region state by parsing and validating the source.
    pub fn new(region: CelRegion, mapper: OffsetMapper) -> Self {
        let result = cel_parser::parse(&region.source);

        // Run validation if we have an AST
        let validation_errors = result
            .ast
            .as_ref()
            .map(|ast| {
                use crate::protovalidate::ProtovalidateResolver;
                use crate::types::validate;
                validate(ast, &ProtovalidateResolver)
            })
            .unwrap_or_default();

        Self {
            region,
            mapper,
            ast: result.ast,
            parse_errors: result.errors,
            validation_errors,
        }
    }

    /// Check if this region contains the given host document offset.
    pub fn contains_host_offset(&self, host_offset: usize) -> bool {
        let start = self.mapper.host_offset();
        let end = start + self.mapper.host_length(self.region.source.len());
        host_offset >= start && host_offset < end
    }

    /// Convert a host offset to a CEL-local offset, if within this region.
    pub fn host_to_cel_offset(&self, host_offset: usize) -> Option<usize> {
        if !self.contains_host_offset(host_offset) {
            return None;
        }

        // Simple case: no escape adjustments
        if self.mapper.escape_adjustments.is_empty() {
            return Some(host_offset - self.mapper.host_offset());
        }

        // With escapes, we need to work backwards from the host offset
        // to find the corresponding CEL offset
        let relative_host = host_offset - self.mapper.host_offset();

        // Binary search to find the CEL offset that maps to this host offset
        // For now, use a simple linear search
        let mut cel_offset = 0;
        let mut adjustment = 0;

        for (threshold, adj) in &self.mapper.escape_adjustments {
            // Check if we've passed the target
            if cel_offset + adjustment >= relative_host {
                break;
            }
            if cel_offset >= *threshold {
                adjustment = *adj;
            }
            cel_offset += 1;
        }

        // Continue until we find the matching position
        while cel_offset + adjustment < relative_host && cel_offset < self.region.source.len() {
            cel_offset += 1;
            adjustment = self.mapper.get_adjustment_at(cel_offset);
        }

        Some(cel_offset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_offset_mapping() {
        let mapper = OffsetMapper::simple(100);
        assert_eq!(mapper.to_host(0), 100);
        assert_eq!(mapper.to_host(10), 110);
        assert_eq!(mapper.to_host(50), 150);
    }

    #[test]
    fn span_mapping() {
        let mapper = OffsetMapper::simple(100);
        let span = 5..15;
        let host_span = mapper.span_to_host(&span);
        assert_eq!(host_span, 105..115);
    }

    #[test]
    fn offset_mapping_with_escapes() {
        // Simulate a string where:
        // - At CEL offset 5, there was a \" escape (1 extra byte in host)
        // - At CEL offset 10, there was another \" escape (2 total extra bytes)
        let mapper = OffsetMapper::new(100, vec![(5, 1), (10, 2)]);

        // Before first escape
        assert_eq!(mapper.to_host(0), 100);
        assert_eq!(mapper.to_host(4), 104);

        // At and after first escape
        assert_eq!(mapper.to_host(5), 106); // 100 + 5 + 1
        assert_eq!(mapper.to_host(9), 110); // 100 + 9 + 1

        // At and after second escape
        assert_eq!(mapper.to_host(10), 112); // 100 + 10 + 2
        assert_eq!(mapper.to_host(15), 117); // 100 + 15 + 2
    }

    #[test]
    fn contains_host_offset() {
        let region = CelRegion {
            source: "this.isEmail()".to_string(),
        };
        let mapper = OffsetMapper::simple(100);
        let state = CelRegionState {
            region,
            mapper,
            ast: None,
            parse_errors: vec![],
            validation_errors: vec![],
        };

        assert!(state.contains_host_offset(100));
        assert!(state.contains_host_offset(105));
        assert!(state.contains_host_offset(113)); // last char
        assert!(!state.contains_host_offset(114)); // one past end
        assert!(!state.contains_host_offset(99)); // before start
    }
}
