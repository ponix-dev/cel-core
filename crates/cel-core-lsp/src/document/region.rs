//! CEL region extraction and offset mapping for embedded CEL expressions.
//!
//! This module provides types for tracking CEL expressions embedded within
//! host documents (like .proto files) and mapping between CEL-local coordinates
//! and host document coordinates.

use std::ops::Range;
use std::sync::Arc;

use cel_core::{parse, CelType, CheckError, CheckResult, Env, ParseError, SpannedExpr};
use cel_core_proto::ProstProtoRegistry;

use crate::protovalidate::ProtovalidateContext;
use crate::settings::protovalidate_extension;

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

    /// Check result from type checking (spans are relative to region).
    pub check_result: Option<CheckResult>,

    /// The environment used for type checking (needed for completion).
    pub env: Arc<Env>,
}

impl CelRegionState {
    /// Create a new CEL region state with a specific protovalidate context.
    pub fn with_context(
        region: CelRegion,
        mapper: OffsetMapper,
        context: ProtovalidateContext,
        proto_registry: Option<&Arc<ProstProtoRegistry>>,
    ) -> Self {
        let result = parse(&region.source);
        let env = Arc::new(build_protovalidate_env_typed(&context, proto_registry));

        // Run type checking if we have an AST
        let check_result = result.ast.as_ref().map(|ast| env.check(ast));

        Self {
            region,
            mapper,
            ast: result.ast,
            parse_errors: result.errors,
            check_result,
            env,
        }
    }

    /// Get the check errors if any.
    pub fn check_errors(&self) -> &[CheckError] {
        self.check_result
            .as_ref()
            .map(|r| r.errors.as_slice())
            .unwrap_or(&[])
    }

    /// Check if this region contains the given host document offset.
    /// Uses `<=` for the end bound so that the cursor at the very end of the
    /// expression (e.g., right before the closing quote) is still considered inside.
    pub fn contains_host_offset(&self, host_offset: usize) -> bool {
        let start = self.mapper.host_offset();
        let end = start + self.mapper.host_length(self.region.source.len());
        host_offset >= start && host_offset <= end
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

/// Resolve a short message name (e.g. "User") to its fully qualified name
/// (e.g. "test.User") using the proto registry's descriptor pool.
///
/// Returns the name as-is if it already contains a dot (already qualified).
/// Returns `None` if the name is ambiguous (multiple matches) or not found.
fn resolve_short_message_name(short_name: &str, registry: &ProstProtoRegistry) -> Option<String> {
    if short_name.contains('.') {
        return Some(short_name.to_string());
    }

    let mut matches = registry
        .pool()
        .all_messages()
        .filter(|msg| msg.name() == short_name);

    let first = matches.next()?;
    // Ambiguous if more than one match
    if matches.next().is_some() {
        return None;
    }
    Some(first.full_name().to_string())
}

/// Resolve `this` type, upgrading short message names to FQ names when a registry is available.
fn resolve_this_type(
    context: &ProtovalidateContext,
    registry: Option<&Arc<ProstProtoRegistry>>,
) -> CelType {
    let this_type = context.this_type();

    // If we have a registry, try to resolve short message names to FQ names
    let Some(registry) = registry else {
        return this_type;
    };

    match &this_type {
        CelType::Message(name) => {
            if let Some(fq_name) = resolve_short_message_name(name.as_ref(), registry) {
                CelType::message(&fq_name)
            } else {
                this_type
            }
        }
        _ => this_type,
    }
}

/// Build a protovalidate environment with typed `this` based on context.
fn build_protovalidate_env_typed(
    context: &ProtovalidateContext,
    proto_registry: Option<&Arc<ProstProtoRegistry>>,
) -> Env {
    let this_type = resolve_this_type(context, proto_registry);

    let mut env = Env::with_standard_library()
        .with_all_extensions()
        .with_extension(protovalidate_extension())
        .with_variable("this", this_type)
        .with_variable("rules", CelType::Dyn)
        .with_variable("now", CelType::Timestamp);

    if let Some(registry) = proto_registry {
        env = env.with_proto_registry(Arc::clone(registry) as Arc<dyn cel_core::ProtoRegistry>);
    }

    env
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
            check_result: None,
            env: Arc::new(Env::new()),
        };

        assert!(state.contains_host_offset(100));
        assert!(state.contains_host_offset(105));
        assert!(state.contains_host_offset(113)); // last char
        assert!(state.contains_host_offset(114)); // cursor at end (before closing quote)
        assert!(!state.contains_host_offset(115)); // one past end
        assert!(!state.contains_host_offset(99)); // before start
    }
}
