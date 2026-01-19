//! Diagnostics conversion from parser and validation errors to LSP diagnostics.

use cel_core_parser::ParseError;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};

use crate::document::{LineIndex, ProtoDocumentState};
use crate::types::{ValidationError, ValidationErrorKind};

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

/// Convert validation errors to LSP diagnostics.
fn validation_errors_to_diagnostics(
    errors: &[ValidationError],
    line_index: &LineIndex,
) -> Vec<Diagnostic> {
    errors
        .iter()
        .map(|error| {
            let code = match error.kind {
                ValidationErrorKind::UndefinedVariable => "undefined-variable",
                ValidationErrorKind::UndefinedMethod => "undefined-method",
                ValidationErrorKind::StandaloneCalledAsMethod => "standalone-as-method",
                ValidationErrorKind::MethodCalledAsStandalone => "method-as-standalone",
                ValidationErrorKind::TooFewArguments => "too-few-arguments",
                ValidationErrorKind::TooManyArguments => "too-many-arguments",
                ValidationErrorKind::InvalidArgumentType => "invalid-argument-type",
            };

            Diagnostic {
                range: line_index.span_to_range(&error.span),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(code.to_string())),
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

/// Convert all errors (parse + validation) to LSP diagnostics.
pub fn to_diagnostics(
    parse_errors: &[ParseError],
    validation_errors: &[ValidationError],
    line_index: &LineIndex,
) -> Vec<Diagnostic> {
    let mut diagnostics = parse_errors_to_diagnostics(parse_errors, line_index);
    diagnostics.extend(validation_errors_to_diagnostics(validation_errors, line_index));
    diagnostics
}

/// Convert all errors from a proto document to LSP diagnostics.
///
/// This processes all CEL regions in the proto document, converting their
/// parse and validation errors with proper offset mapping to host coordinates.
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

        // Convert validation errors
        for error in &region_state.validation_errors {
            let host_span = mapper.span_to_host(&error.span);
            let range = state.line_index.span_to_range(&host_span);
            let code = match error.kind {
                ValidationErrorKind::UndefinedVariable => "undefined-variable",
                ValidationErrorKind::UndefinedMethod => "undefined-method",
                ValidationErrorKind::StandaloneCalledAsMethod => "standalone-as-method",
                ValidationErrorKind::MethodCalledAsStandalone => "method-as-standalone",
                ValidationErrorKind::TooFewArguments => "too-few-arguments",
                ValidationErrorKind::TooManyArguments => "too-many-arguments",
                ValidationErrorKind::InvalidArgumentType => "invalid-argument-type",
            };

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(code.to_string())),
                code_description: None,
                source: Some("cel".to_string()),
                message: error.message.clone(),
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

    #[test]
    fn creates_diagnostic_from_parse_error() {
        let source = "1 + ";
        let result = cel_core_parser::parse(source);
        let line_index = LineIndex::new(source.to_string());

        assert!(!result.errors.is_empty());
        let diagnostics = to_diagnostics(&result.errors, &[], &line_index);

        assert!(!diagnostics.is_empty());
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostics[0].source, Some("cel".to_string()));
    }

    #[test]
    fn creates_diagnostic_from_validation_error() {
        let error = ValidationError {
            kind: ValidationErrorKind::UndefinedVariable,
            message: "undefined variable 'x'".to_string(),
            span: 0..1,
            name: "x".to_string(),
        };
        let line_index = LineIndex::new("x".to_string());

        let diagnostics = to_diagnostics(&[], &[error], &line_index);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(
            diagnostics[0].code,
            Some(NumberOrString::String("undefined-variable".to_string()))
        );
    }
}
