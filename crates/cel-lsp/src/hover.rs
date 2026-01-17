//! Hover information for CEL expressions.

use cel_parser::{Expr, SpannedExpr};
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use cel_types::{get_builtin, BuiltinFunction};
use crate::text::LineIndex;
use crate::validation::{ValidationError, ValidationErrorKind};

/// Format builtin function documentation as markdown.
fn format_builtin_docs(builtin: &BuiltinFunction) -> String {
    let mut doc = format!("**{}**`{}`\n\n{}", builtin.name, builtin.signature, builtin.description);
    if let Some(example) = builtin.example {
        doc.push_str(&format!("\n\n*Example:* `{}`", example));
    }
    doc
}

/// Find the AST node at a given position.
fn find_node_at_position<'a>(
    line_index: &LineIndex,
    ast: &'a SpannedExpr,
    position: Position,
) -> Option<&'a SpannedExpr> {
    let target_offset = line_index.position_to_offset(position)?;
    find_node_containing_offset(ast, target_offset)
}

/// Find the innermost node containing the given offset.
fn find_node_containing_offset<'a>(ast: &'a SpannedExpr, offset: usize) -> Option<&'a SpannedExpr> {
    if !ast.span.contains(&offset) {
        return None;
    }

    // Try to find a more specific child node
    let child = match &ast.node {
        Expr::Null | Expr::Bool(_) | Expr::Int(_) | Expr::UInt(_) | Expr::Float(_) => None,
        Expr::String(_) | Expr::Bytes(_) | Expr::Ident(_) | Expr::RootIdent(_) => None,
        Expr::List(items) => items
            .iter()
            .find_map(|item| find_node_containing_offset(item, offset)),
        Expr::Map(entries) => entries.iter().find_map(|(k, v)| {
            find_node_containing_offset(k, offset).or_else(|| find_node_containing_offset(v, offset))
        }),
        Expr::Unary { expr, .. } => find_node_containing_offset(expr, offset),
        Expr::Binary { left, right, .. } => find_node_containing_offset(left, offset)
            .or_else(|| find_node_containing_offset(right, offset)),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => find_node_containing_offset(cond, offset)
            .or_else(|| find_node_containing_offset(then_expr, offset))
            .or_else(|| find_node_containing_offset(else_expr, offset)),
        Expr::Member { expr, .. } => find_node_containing_offset(expr, offset),
        Expr::Index { expr, index } => find_node_containing_offset(expr, offset)
            .or_else(|| find_node_containing_offset(index, offset)),
        Expr::Call { expr, args } => find_node_containing_offset(expr, offset)
            .or_else(|| args.iter().find_map(|arg| find_node_containing_offset(arg, offset))),
        Expr::Struct { type_name, fields } => find_node_containing_offset(type_name, offset)
            .or_else(|| fields.iter().find_map(|(_, v)| find_node_containing_offset(v, offset))),
        Expr::Error => None,
    };

    child.or(Some(ast))
}

/// Format a validation error for hover display.
fn format_validation_error(error: &ValidationError) -> String {
    match error.kind {
        ValidationErrorKind::UndefinedVariable => {
            format!(
                "**Error:** Undefined variable `{}`\n\n\
                 This variable is not defined in the current context.",
                error.name
            )
        }
        ValidationErrorKind::UndefinedMethod => {
            format!(
                "**Error:** Undefined method `{}`\n\n\
                 This method is not a CEL builtin.",
                error.name
            )
        }
    }
}

/// Find a validation error that overlaps with the given node.
fn find_validation_error_at<'a>(
    node: &SpannedExpr,
    errors: &'a [ValidationError],
) -> Option<&'a ValidationError> {
    errors.iter().find(|e| {
        // Check if error span overlaps with node span
        e.span.start < node.span.end && e.span.end > node.span.start
    })
}

/// Generate hover information for a node.
/// Checks validation errors first, then falls back to builtin docs.
fn hover_for_node(
    line_index: &LineIndex,
    node: &SpannedExpr,
    validation_errors: &[ValidationError],
) -> Option<Hover> {
    // Check if this node has a validation error
    if let Some(error) = find_validation_error_at(node, validation_errors) {
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format_validation_error(error),
            }),
            range: Some(line_index.span_to_range(&error.span)),
        });
    }

    // Fall back to builtin documentation
    let description = match &node.node {
        Expr::Ident(name) => get_builtin(name).map(format_builtin_docs),
        Expr::Member { field, .. } => get_builtin(field).map(format_builtin_docs),
        Expr::Call { expr, .. } => match &expr.node {
            Expr::Ident(name) => get_builtin(name).map(format_builtin_docs),
            Expr::Member { field, .. } => get_builtin(field).map(format_builtin_docs),
            _ => None,
        },
        _ => None,
    }?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: description,
        }),
        range: Some(line_index.span_to_range(&node.span)),
    })
}

/// Get hover information for a position in the document.
pub fn hover_at_position(
    line_index: &LineIndex,
    ast: &SpannedExpr,
    validation_errors: &[ValidationError],
    position: Position,
) -> Option<Hover> {
    let node = find_node_at_position(line_index, ast, position)?;
    hover_for_node(line_index, node, validation_errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_hover_for_number() {
        let source = "42";
        let result = cel_parser::parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let hover = hover_at_position(&line_index, &ast, &[], Position::new(0, 0));
        assert!(hover.is_none());
    }

    #[test]
    fn hover_for_function_call() {
        let source = "size(x)";
        let result = cel_parser::parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let hover = hover_at_position(&line_index, &ast, &[], Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        match hover.contents {
            HoverContents::Markup(m) => {
                assert!(m.value.contains("size"));
            }
            _ => panic!("Expected markup content"),
        }
    }

    #[test]
    fn hover_for_undefined_variable() {
        let source = "x";
        let result = cel_parser::parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());
        let validation_errors = vec![ValidationError {
            kind: ValidationErrorKind::UndefinedVariable,
            message: "undefined variable 'x'".to_string(),
            span: 0..1,
            name: "x".to_string(),
        }];

        let hover = hover_at_position(&line_index, &ast, &validation_errors, Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        match hover.contents {
            HoverContents::Markup(m) => {
                assert!(m.value.contains("Undefined variable"));
                assert!(m.value.contains("`x`"));
            }
            _ => panic!("Expected markup content"),
        }
    }
}
