//! Diagnostics conversion from parser and check errors to LSP diagnostics.

use cel_core::{CheckError, CheckErrorKind, ParseError};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};

use crate::document::{LineIndex, ProtoDocumentState};

/// Convert parser errors to LSP diagnostics.
fn parse_errors_to_diagnostics(errors: &[ParseError], line_index: &LineIndex) -> Vec<Diagnostic> {
    errors
        .iter()
        .map(|error| {
            let range = line_index.span_to_range(&error.span);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("cel".to_string()),
                message: error.message.clone(),
                related_information: None,
                tags: None,
                data: None,
            }
        })
        .collect()
}

/// Convert check errors to LSP diagnostics.
fn check_errors_to_diagnostics(errors: &[CheckError], line_index: &LineIndex) -> Vec<Diagnostic> {
    errors
        .iter()
        .map(|error| {
            let code = match &error.kind {
                CheckErrorKind::UndeclaredReference { .. } => "undeclared-reference",
                CheckErrorKind::NoMatchingOverload { .. } => "no-matching-overload",
                CheckErrorKind::TypeMismatch { .. } => "type-mismatch",
                CheckErrorKind::UndefinedField { .. } => "undefined-field",
                CheckErrorKind::NotAssignable { .. } => "type-mismatch",
                CheckErrorKind::HeterogeneousAggregate { .. } => "heterogeneous-aggregate",
                CheckErrorKind::NotAType { .. } => "not-a-type",
                CheckErrorKind::Other(_) => "check-error",
            };

            Diagnostic {
                range: line_index.span_to_range(&error.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(code.to_string())),
                code_description: None,
                source: Some("cel".to_string()),
                message: error.message(),
                related_information: None,
                tags: None,
                data: None,
            }
        })
        .collect()
}

/// Convert all errors (parse + check) to LSP diagnostics.
pub fn to_diagnostics(
    parse_errors: &[ParseError],
    check_errors: &[CheckError],
    line_index: &LineIndex,
) -> Vec<Diagnostic> {
    let mut diagnostics = parse_errors_to_diagnostics(parse_errors, line_index);
    diagnostics.extend(check_errors_to_diagnostics(check_errors, line_index));
    diagnostics
}

/// Convert all errors from a proto document to LSP diagnostics.
///
/// This processes all CEL regions in the proto document, converting their
/// parse and check errors with proper offset mapping to host coordinates.
pub fn proto_to_diagnostics(state: &ProtoDocumentState) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for region_state in &state.regions {
        let mapper = &region_state.mapper;

        // Convert parse errors
        for error in &region_state.parse_errors {
            let host_span = mapper.span_to_host(&error.span);
            let range = state.line_index.span_to_range(&host_span);
            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("cel".to_string()),
                message: error.message.clone(),
                related_information: None,
                tags: None,
                data: None,
            });
        }

        // Convert check errors
        for error in region_state.check_errors() {
            let host_span = mapper.span_to_host(&error.span);
            let range = state.line_index.span_to_range(&host_span);
            let code = match &error.kind {
                CheckErrorKind::UndeclaredReference { .. } => "undeclared-reference",
                CheckErrorKind::NoMatchingOverload { .. } => "no-matching-overload",
                CheckErrorKind::TypeMismatch { .. } => "type-mismatch",
                CheckErrorKind::UndefinedField { .. } => "undefined-field",
                CheckErrorKind::NotAssignable { .. } => "type-mismatch",
                CheckErrorKind::HeterogeneousAggregate { .. } => "heterogeneous-aggregate",
                CheckErrorKind::NotAType { .. } => "not-a-type",
                CheckErrorKind::Other(_) => "check-error",
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(code.to_string())),
                code_description: None,
                source: Some("cel".to_string()),
                message: error.message(),
                related_information: None,
                tags: None,
                data: None,
            });
        }
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core::parse;

    #[test]
    fn creates_diagnostic_from_parse_error() {
        let source = "1 + ";
        let result = parse(source);
        let line_index = LineIndex::new(source.to_string());

        assert!(!result.errors.is_empty());
        let diagnostics = to_diagnostics(&result.errors, &[], &line_index);

        assert!(!diagnostics.is_empty());
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostics[0].source, Some("cel".to_string()));
    }

    #[test]
    fn creates_diagnostic_from_check_error() {
        let error = CheckError::undeclared_reference("x", 0..1, 1);
        let line_index = LineIndex::new("x".to_string());

        let diagnostics = to_diagnostics(&[], &[error], &line_index);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(
            diagnostics[0].code,
            Some(NumberOrString::String("undeclared-reference".to_string()))
        );
    }
}
