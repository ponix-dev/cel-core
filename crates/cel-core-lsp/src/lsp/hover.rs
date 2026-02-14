//! Hover information for CEL expressions.

use cel_core::{types::Expr, CheckError, CheckErrorKind, CheckResult, SpannedExpr};
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use crate::document::{LineIndex, ProtoDocumentState};
use crate::protovalidate::get_protovalidate_builtin;
use crate::types::{get_builtin, FunctionDef};

/// Format builtin function documentation as markdown.
fn format_builtin_docs(builtin: &FunctionDef) -> String {
    let mut doc = format!(
        "**{}**`{}`\n\n{}",
        builtin.name, builtin.signature, builtin.description
    );
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
fn find_node_containing_offset(ast: &SpannedExpr, offset: usize) -> Option<&SpannedExpr> {
    if !ast.span.contains(&offset) {
        return None;
    }

    // Try to find a more specific child node
    let child = match &ast.node {
        Expr::Null | Expr::Bool(_) | Expr::Int(_) | Expr::UInt(_) | Expr::Float(_) => None,
        Expr::String(_) | Expr::Bytes(_) | Expr::Ident(_) | Expr::RootIdent(_) => None,
        Expr::List(items) => items
            .iter()
            .find_map(|item| find_node_containing_offset(&item.expr, offset)),
        Expr::Map(entries) => entries.iter().find_map(|entry| {
            find_node_containing_offset(&entry.key, offset)
                .or_else(|| find_node_containing_offset(&entry.value, offset))
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
        Expr::Index { expr, index, .. } => find_node_containing_offset(expr, offset)
            .or_else(|| find_node_containing_offset(index, offset)),
        Expr::Call { expr, args } => find_node_containing_offset(expr, offset).or_else(|| {
            args.iter()
                .find_map(|arg| find_node_containing_offset(arg, offset))
        }),
        Expr::Struct { type_name, fields } => find_node_containing_offset(type_name, offset)
            .or_else(|| {
                fields
                    .iter()
                    .find_map(|field| find_node_containing_offset(&field.value, offset))
            }),
        Expr::Comprehension(comp) => find_node_containing_offset(&comp.iter_range, offset)
            .or_else(|| find_node_containing_offset(&comp.accu_init, offset))
            .or_else(|| find_node_containing_offset(&comp.loop_condition, offset))
            .or_else(|| find_node_containing_offset(&comp.loop_step, offset))
            .or_else(|| find_node_containing_offset(&comp.result, offset)),
        Expr::MemberTestOnly { expr, .. } => find_node_containing_offset(expr, offset),
        Expr::Bind { init, body, .. } => find_node_containing_offset(init, offset)
            .or_else(|| find_node_containing_offset(body, offset)),
        Expr::Error => None,
    };

    child.or(Some(ast))
}

/// Format a check error for hover display.
fn format_check_error(error: &CheckError) -> String {
    match &error.kind {
        CheckErrorKind::UndeclaredReference { name, .. } => {
            format!(
                "**Error:** Undeclared reference `{}`\n\n\
                 This variable or function is not defined in the current context.",
                name
            )
        }
        CheckErrorKind::NoMatchingOverload {
            function,
            arg_types,
        } => {
            let types: Vec<_> = arg_types.iter().map(|t| t.display_name()).collect();
            format!(
                "**Error:** No matching overload for `{}`\n\n\
                 No overload found with argument types ({}).",
                function,
                types.join(", ")
            )
        }
        CheckErrorKind::TypeMismatch { expected, actual } => {
            format!(
                "**Error:** Type mismatch\n\n\
                 Expected `{}` but found `{}`.",
                expected.display_name(),
                actual.display_name()
            )
        }
        CheckErrorKind::UndefinedField { type_name, field } => {
            format!(
                "**Error:** Undefined field `{}`\n\n\
                 The type `{}` has no field named `{}`.",
                field, type_name, field
            )
        }
        CheckErrorKind::NotAssignable { from, to } => {
            format!(
                "**Error:** Type not assignable\n\n\
                 Type `{}` is not assignable to `{}`.",
                from.display_name(),
                to.display_name()
            )
        }
        CheckErrorKind::HeterogeneousAggregate { types } => {
            let type_names: Vec<_> = types.iter().map(|t| t.display_name()).collect();
            format!(
                "**Error:** Heterogeneous aggregate\n\n\
                 Aggregate literal contains mixed types: {}.",
                type_names.join(", ")
            )
        }
        CheckErrorKind::NotAType { expr } => {
            format!(
                "**Error:** Not a type\n\n\
                 `{}` cannot be used as a type.",
                expr
            )
        }
        CheckErrorKind::Other(msg) => {
            format!("**Error:** {}", msg)
        }
    }
}

/// Find a check error that overlaps with the given node.
fn find_check_error_at<'a>(node: &SpannedExpr, errors: &'a [CheckError]) -> Option<&'a CheckError> {
    errors.iter().find(|e| {
        // Check if error span overlaps with node span
        e.span.start < node.span.end && e.span.end > node.span.start
    })
}

/// Generate hover information for a node.
/// Checks check errors first, then variable types, then falls back to builtin docs.
fn hover_for_node(
    line_index: &LineIndex,
    node: &SpannedExpr,
    check_result: Option<&CheckResult>,
) -> Option<Hover> {
    let check_errors = check_result.map(|r| r.errors.as_slice()).unwrap_or(&[]);

    // Check if this node has a check error
    if let Some(error) = find_check_error_at(node, check_errors) {
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format_check_error(error),
            }),
            range: Some(line_index.span_to_range(&error.span)),
        });
    }

    // Show variable type for identifiers
    if let Some(check_result) = check_result {
        let var_name = match &node.node {
            Expr::Ident(name) => Some(name.as_str()),
            Expr::RootIdent(name) => Some(name.as_str()),
            _ => None,
        };
        if let Some(name) = var_name {
            if let Some(cel_type) = check_result.type_map.get(&node.id) {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("(variable) `{}`: `{}`", name, cel_type.display_name()),
                    }),
                    range: Some(line_index.span_to_range(&node.span)),
                });
            }
        }
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
        Expr::MemberTestOnly { .. } => get_builtin("has").map(format_builtin_docs),
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
    check_result: Option<&CheckResult>,
    position: Position,
) -> Option<Hover> {
    let node = find_node_at_position(line_index, ast, position)?;
    hover_for_node(line_index, node, check_result)
}

/// Get hover information for a position in a proto document.
///
/// This finds the CEL region at the given position, locates the AST node,
/// and returns hover information with proper offset mapping.
pub fn hover_at_position_proto(state: &ProtoDocumentState, position: Position) -> Option<Hover> {
    // Convert position to byte offset in host document
    let host_offset = state.line_index.position_to_offset(position)?;

    // Find which CEL region contains this offset
    let region_state = state.region_at_offset(host_offset)?;

    // Convert host offset to CEL-local offset
    let cel_offset = region_state.host_to_cel_offset(host_offset)?;

    // Find AST node at the CEL-local offset
    let ast = region_state.ast.as_ref()?;
    let node = find_node_containing_offset(ast, cel_offset)?;

    let check_result = region_state.check_result.as_ref();
    let check_errors = check_result.map(|r| r.errors.as_slice()).unwrap_or(&[]);

    // Check if this node has a check error
    if let Some(error) = find_check_error_at(node, check_errors) {
        let host_span = region_state.mapper.span_to_host(&error.span);
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format_check_error(error),
            }),
            range: Some(state.line_index.span_to_range(&host_span)),
        });
    }

    // Show variable type for identifiers
    if let Some(check_result) = check_result {
        let var_name = match &node.node {
            Expr::Ident(name) => Some(name.as_str()),
            Expr::RootIdent(name) => Some(name.as_str()),
            _ => None,
        };
        if let Some(name) = var_name {
            if let Some(cel_type) = check_result.type_map.get(&node.id) {
                let host_span = region_state.mapper.span_to_host(&node.span);
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("(variable) `{}`: `{}`", name, cel_type.display_name()),
                    }),
                    range: Some(state.line_index.span_to_range(&host_span)),
                });
            }
        }
    }

    // Fall back to builtin documentation (including protovalidate functions for proto files)
    let get_function_docs = |name: &str| -> Option<String> {
        get_builtin(name)
            .or_else(|| get_protovalidate_builtin(name))
            .map(format_builtin_docs)
    };

    let description = match &node.node {
        Expr::Ident(name) => get_function_docs(name),
        Expr::Member { field, .. } => get_function_docs(field),
        Expr::Call { expr, .. } => match &expr.node {
            Expr::Ident(name) => get_function_docs(name),
            Expr::Member { field, .. } => get_function_docs(field),
            _ => None,
        },
        Expr::MemberTestOnly { .. } => get_function_docs("has"),
        _ => None,
    }?;

    let host_span = region_state.mapper.span_to_host(&node.span);
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: description,
        }),
        range: Some(state.line_index.span_to_range(&host_span)),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core::{parse, CelType, Env};

    #[test]
    fn no_hover_for_number() {
        let source = "42";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let hover = hover_at_position(&line_index, &ast, None, Position::new(0, 0));
        assert!(hover.is_none());
    }

    #[test]
    fn hover_for_function_call() {
        let source = "size(x)";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let hover = hover_at_position(&line_index, &ast, None, Position::new(0, 0));
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
    fn hover_for_undeclared_reference() {
        let source = "x";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());
        let check_result = CheckResult {
            type_map: Default::default(),
            reference_map: Default::default(),
            errors: vec![CheckError::undeclared_reference("x", 0..1, 1)],
        };

        let hover = hover_at_position(&line_index, &ast, Some(&check_result), Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        match hover.contents {
            HoverContents::Markup(m) => {
                assert!(m.value.contains("Undeclared reference"));
                assert!(m.value.contains("`x`"));
            }
            _ => panic!("Expected markup content"),
        }
    }

    #[test]
    fn hover_for_has_macro() {
        let source = "has(msg.field)";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        // Hover on "has" (position 0) should return has() builtin docs
        let hover = hover_at_position(&line_index, &ast, None, Position::new(0, 0));
        assert!(hover.is_some());
        let hover = hover.unwrap();
        match hover.contents {
            HoverContents::Markup(m) => {
                assert!(m.value.contains("has"));
            }
            _ => panic!("Expected markup content"),
        }
    }

    #[test]
    fn hover_for_variable_type() {
        let source = "x > 10";
        let env = Env::with_standard_library().with_variable("x", CelType::Int);
        let ast = env.compile(source).unwrap();
        let line_index = LineIndex::new(source.to_string());
        let check_result = ast.type_info().unwrap();

        // Hover on "x" (position 0) should show variable type
        let hover = hover_at_position(
            &line_index,
            ast.expr(),
            Some(check_result),
            Position::new(0, 0),
        );
        assert!(hover.is_some());
        let hover = hover.unwrap();
        match hover.contents {
            HoverContents::Markup(m) => {
                assert!(
                    m.value.contains("(variable) `x`: `int`"),
                    "Expected variable type hover, got: {}",
                    m.value
                );
            }
            _ => panic!("Expected markup content"),
        }
    }

    #[test]
    fn hover_for_message_variable() {
        let source = "this";
        let env = Env::with_standard_library()
            .with_variable("this", CelType::Message("my.package.MyMessage".into()));
        let ast = env.compile(source).unwrap();
        let line_index = LineIndex::new(source.to_string());
        let check_result = ast.type_info().unwrap();

        let hover = hover_at_position(
            &line_index,
            ast.expr(),
            Some(check_result),
            Position::new(0, 0),
        );
        assert!(hover.is_some());
        let hover = hover.unwrap();
        match hover.contents {
            HoverContents::Markup(m) => {
                assert!(
                    m.value
                        .contains("(variable) `this`: `my.package.MyMessage`"),
                    "Expected message type hover, got: {}",
                    m.value
                );
            }
            _ => panic!("Expected markup content"),
        }
    }
}
