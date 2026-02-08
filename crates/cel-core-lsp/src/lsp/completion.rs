//! Completion support for CEL expressions.
//!
//! Provides type-aware autocomplete by inserting a placeholder identifier at the
//! cursor position, re-parsing and re-checking the expression, then generating
//! completion items based on the inferred type context.

use cel_core::types::Expr;
use cel_core::{CelType, Env, SpannedExpr};
use tower_lsp::lsp_types::*;

use crate::document::{LineIndex, ProtoDocumentState};
use crate::protovalidate::PROTOVALIDATE_BUILTINS;
use crate::types::{get_builtin, FunctionDef};

/// Placeholder identifier inserted at the cursor for re-checking.
const PLACEHOLDER: &str = "__cel_complete__";

/// CEL macro names that should appear in identifier completion.
const MACROS: &[&str] = &["has", "all", "exists", "exists_one", "filter", "map"];

/// What kind of completion context we detected.
#[derive(Debug)]
enum CompletionContext {
    /// Cursor is after a `.` — need receiver type for member suggestions.
    /// Contains the prefix text the user has typed after the dot (may be empty).
    MemberAccess { prefix: String },
    /// Cursor is at a bare or partial identifier — suggest variables + functions.
    /// Contains the partial identifier text (may be empty).
    Identifier { prefix: String },
}

/// Detect the completion context by scanning backwards from the cursor.
fn detect_context(source: &str, offset: usize) -> CompletionContext {
    let before = &source[..offset];

    // Scan backwards to find any partial identifier being typed
    let ident_start = before
        .bytes()
        .rev()
        .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
        .count();
    let prefix = &before[before.len() - ident_start..];

    // Check if there's a dot before the partial identifier
    let before_prefix = &before[..before.len() - ident_start];
    let trimmed = before_prefix.trim_end();

    if trimmed.ends_with('.') {
        CompletionContext::MemberAccess {
            prefix: prefix.to_string(),
        }
    } else {
        CompletionContext::Identifier {
            prefix: prefix.to_string(),
        }
    }
}

/// Find the Expr::Member node with our placeholder field in the AST.
fn find_placeholder_member(ast: &SpannedExpr) -> Option<&SpannedExpr> {
    match &ast.node {
        Expr::Member { expr, field, .. } if field == PLACEHOLDER => Some(ast),
        // Recurse into children
        Expr::Member { expr, .. } => find_placeholder_member(expr),
        Expr::Call { expr, args } => {
            find_placeholder_member(expr).or_else(|| args.iter().find_map(find_placeholder_member))
        }
        Expr::Binary { left, right, .. } => {
            find_placeholder_member(left).or_else(|| find_placeholder_member(right))
        }
        Expr::Unary { expr, .. } => find_placeholder_member(expr),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => find_placeholder_member(cond)
            .or_else(|| find_placeholder_member(then_expr))
            .or_else(|| find_placeholder_member(else_expr)),
        Expr::Index { expr, index, .. } => {
            find_placeholder_member(expr).or_else(|| find_placeholder_member(index))
        }
        Expr::List(items) => items
            .iter()
            .find_map(|item| find_placeholder_member(&item.expr)),
        Expr::Map(entries) => entries.iter().find_map(|entry| {
            find_placeholder_member(&entry.key).or_else(|| find_placeholder_member(&entry.value))
        }),
        Expr::Comprehension(comp) => find_placeholder_member(&comp.iter_range)
            .or_else(|| find_placeholder_member(&comp.accu_init))
            .or_else(|| find_placeholder_member(&comp.loop_condition))
            .or_else(|| find_placeholder_member(&comp.loop_step))
            .or_else(|| find_placeholder_member(&comp.result)),
        Expr::MemberTestOnly { expr, field } if field == PLACEHOLDER => Some(ast),
        Expr::MemberTestOnly { expr, .. } => find_placeholder_member(expr),
        Expr::Bind { init, body, .. } => {
            find_placeholder_member(init).or_else(|| find_placeholder_member(body))
        }
        Expr::Struct { fields, .. } => fields
            .iter()
            .find_map(|f| find_placeholder_member(&f.value)),
        _ => None,
    }
}

/// Get the receiver expression ID from a Member node containing our placeholder.
fn get_receiver_id(node: &SpannedExpr) -> Option<i64> {
    match &node.node {
        Expr::Member { expr, field, .. } if field == PLACEHOLDER => Some(expr.id),
        Expr::MemberTestOnly { expr, field } if field == PLACEHOLDER => Some(expr.id),
        _ => None,
    }
}

/// Resolve the receiver type by inserting a placeholder and re-checking.
///
/// `prefix_len` is the length of any partial identifier already typed after the dot.
/// We strip that prefix and everything after the cursor, then append the placeholder
/// so the parser sees a clean `receiver.__cel_complete__` expression.
fn resolve_receiver_type(
    source: &str,
    offset: usize,
    prefix_len: usize,
    env: &Env,
) -> Option<CelType> {
    // Insert the placeholder right after the dot, replacing any partial text
    // but keeping the rest of the source (closing parens, etc.) so macros like
    // has() can still be expanded correctly.
    let insert_offset = offset - prefix_len;
    let modified = format!(
        "{}{}{}",
        &source[..insert_offset],
        PLACEHOLDER,
        &source[offset..]
    );

    let parse_result = env.parse(&modified);
    let ast = parse_result.ast?;
    let check_result = env.check(&ast);

    // Find the placeholder Member node
    let member_node = find_placeholder_member(&ast)?;
    let receiver_id = get_receiver_id(member_node)?;

    // Look up the receiver's type in the type map
    check_result.type_map.get(&receiver_id).cloned()
}

/// Format a function overload as a detail string.
fn format_overload_detail(overload: &cel_core::types::OverloadDecl) -> String {
    let args: Vec<String> = overload
        .arg_types()
        .iter()
        .map(|t| t.display_name())
        .collect();
    let result = overload.result.display_name();
    format!("({}) -> {}", args.join(", "), result)
}

/// Build completion items for member access on a known type.
fn member_completions(
    receiver_type: &CelType,
    env: &Env,
    prefix: &str,
    is_proto: bool,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Add proto message fields if the receiver is a message type
    if let CelType::Message(msg_name) = receiver_type {
        if let Some(registry) = env.proto_registry() {
            if let Some(fields) = registry.message_field_names(msg_name) {
                for field_name in fields {
                    if !prefix.is_empty()
                        && !field_name
                            .to_lowercase()
                            .starts_with(&prefix.to_lowercase())
                    {
                        continue;
                    }
                    let field_type = registry
                        .get_field_type(msg_name, &field_name)
                        .map(|t| t.display_name())
                        .unwrap_or_default();
                    items.push(CompletionItem {
                        label: field_name.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: if field_type.is_empty() {
                            None
                        } else {
                            Some(field_type)
                        },
                        sort_text: Some(format!("0_{}", field_name)),
                        ..Default::default()
                    });
                }
            }
        }
    }

    // Add member methods compatible with the receiver type
    let methods = env.methods_for_type(receiver_type);
    for (name, overload) in &methods {
        // Skip operator functions
        if name.starts_with('_') || name.contains('@') {
            continue;
        }
        if !prefix.is_empty() && !name.to_lowercase().starts_with(&prefix.to_lowercase()) {
            continue;
        }
        let detail = format_overload_detail(overload);

        // Build snippet with arg placeholders
        let arg_types = overload.arg_types();
        let insert_text = if arg_types.is_empty() {
            format!("{}()", name)
        } else {
            let placeholders: Vec<String> = arg_types
                .iter()
                .enumerate()
                .map(|(i, _)| format!("${{{}}}", i + 1))
                .collect();
            format!("{}({})", name, placeholders.join(", "))
        };

        // Look up documentation from builtins
        let documentation = get_function_docs(name, is_proto);

        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::METHOD),
            detail: Some(detail),
            documentation,
            insert_text: Some(insert_text),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some(format!("1_{}", name)),
            ..Default::default()
        });
    }

    // For Dyn type, add all member functions since we can't narrow
    if matches!(receiver_type, CelType::Dyn) {
        // Already covered by methods_for_type since Dyn is assignable from everything
    }

    items
}

/// Build completion items for bare identifiers.
fn identifier_completions(env: &Env, prefix: &str, is_proto: bool) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Add variables
    for (name, cel_type) in env.variables() {
        if !prefix.is_empty() && !name.to_lowercase().starts_with(&prefix.to_lowercase()) {
            continue;
        }
        items.push(CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(cel_type.display_name()),
            sort_text: Some(format!("0_{}", name)),
            ..Default::default()
        });
    }

    // Add standalone functions
    let functions = env.standalone_functions();
    for name in functions {
        if !prefix.is_empty() && !name.to_lowercase().starts_with(&prefix.to_lowercase()) {
            continue;
        }

        // Look up documentation
        let documentation = get_function_docs(name, is_proto);

        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            documentation,
            sort_text: Some(format!("1_{}", name)),
            ..Default::default()
        });
    }

    // Add macros
    for &name in MACROS {
        if !prefix.is_empty() && !name.to_lowercase().starts_with(&prefix.to_lowercase()) {
            continue;
        }
        let documentation = get_function_docs(name, is_proto);
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            documentation,
            sort_text: Some(format!("2_{}", name)),
            ..Default::default()
        });
    }

    items
}

/// Look up documentation for a function from builtins or protovalidate builtins.
fn get_function_docs(name: &str, is_proto: bool) -> Option<Documentation> {
    let builtin: Option<&FunctionDef> = get_builtin(name).or_else(|| {
        if is_proto {
            PROTOVALIDATE_BUILTINS.get(name)
        } else {
            None
        }
    });
    builtin.map(|b| {
        let mut doc = format!("{}\n\n{}", b.signature, b.description);
        if let Some(example) = b.example {
            doc.push_str(&format!("\n\nExample: `{}`", example));
        }
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc,
        })
    })
}

/// Generate completions at a position in a CEL expression.
pub fn completion_at_position(
    line_index: &LineIndex,
    source: &str,
    env: &Env,
    position: Position,
) -> Option<CompletionResponse> {
    let offset = line_index.position_to_offset(position)?;
    let context = detect_context(source, offset);

    let items = match context {
        CompletionContext::MemberAccess { prefix } => {
            let receiver_type = resolve_receiver_type(source, offset, prefix.len(), env);
            match receiver_type {
                Some(ty) => member_completions(&ty, env, &prefix, false),
                None => member_completions(&CelType::Dyn, env, &prefix, false),
            }
        }
        CompletionContext::Identifier { prefix } => identifier_completions(env, &prefix, false),
    };

    if items.is_empty() {
        None
    } else {
        Some(CompletionResponse::Array(items))
    }
}

/// Generate completions at a position in a proto document.
pub fn completion_at_position_proto(
    state: &ProtoDocumentState,
    position: Position,
) -> Option<CompletionResponse> {
    let host_offset = state.line_index.position_to_offset(position)?;
    let region_state = state.region_at_offset(host_offset)?;
    let cel_offset = region_state.host_to_cel_offset(host_offset)?;

    let source = &region_state.region.source;
    let env = &region_state.env;
    let context = detect_context(source, cel_offset);

    let items = match context {
        CompletionContext::MemberAccess { prefix } => {
            let receiver_type = resolve_receiver_type(source, cel_offset, prefix.len(), env);
            match receiver_type {
                Some(ty) => member_completions(&ty, env, &prefix, true),
                None => member_completions(&CelType::Dyn, env, &prefix, true),
            }
        }
        CompletionContext::Identifier { prefix } => identifier_completions(env, &prefix, true),
    };

    if items.is_empty() {
        None
    } else {
        Some(CompletionResponse::Array(items))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core::Env;

    fn get_completions(source: &str, position: Position) -> Vec<CompletionItem> {
        let env = Env::with_standard_library().with_all_extensions();
        let line_index = LineIndex::new(source.to_string());
        match completion_at_position(&line_index, source, &env, position) {
            Some(CompletionResponse::Array(items)) => items,
            _ => vec![],
        }
    }

    fn get_completions_with_env(
        source: &str,
        position: Position,
        env: &Env,
    ) -> Vec<CompletionItem> {
        let line_index = LineIndex::new(source.to_string());
        match completion_at_position(&line_index, source, env, position) {
            Some(CompletionResponse::Array(items)) => items,
            _ => vec![],
        }
    }

    fn labels(items: &[CompletionItem]) -> Vec<&str> {
        items.iter().map(|i| i.label.as_str()).collect()
    }

    #[test]
    fn bare_identifier_suggests_variables() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("myVar", CelType::Int)
            .with_variable("myOther", CelType::String);

        let items = get_completions_with_env("my", Position::new(0, 2), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"myVar"),
            "should suggest myVar: {:?}",
            names
        );
        assert!(
            names.contains(&"myOther"),
            "should suggest myOther: {:?}",
            names
        );
    }

    #[test]
    fn bare_identifier_suggests_functions() {
        let items = get_completions("si", Position::new(0, 2));
        let names = labels(&items);
        assert!(names.contains(&"size"), "should suggest size: {:?}", names);
    }

    #[test]
    fn member_access_on_string() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("name", CelType::String);

        // `name.` — cursor after the dot
        let items = get_completions_with_env("name.", Position::new(0, 5), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"contains"),
            "should suggest contains: {:?}",
            names
        );
        assert!(
            names.contains(&"startsWith"),
            "should suggest startsWith: {:?}",
            names
        );
        assert!(
            names.contains(&"endsWith"),
            "should suggest endsWith: {:?}",
            names
        );
        assert!(names.contains(&"size"), "should suggest size: {:?}", names);
        assert!(
            names.contains(&"matches"),
            "should suggest matches: {:?}",
            names
        );
    }

    #[test]
    fn member_access_on_list() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("items", CelType::list(CelType::Int));

        let items = get_completions_with_env("items.", Position::new(0, 6), &env);
        let names = labels(&items);
        assert!(names.contains(&"size"), "should suggest size: {:?}", names);
    }

    #[test]
    fn member_access_mid_expression() {
        // Reproduces the bug: cursor after `this.` in `this.isEmail()`
        // The placeholder was concatenating with `isEmail` instead of replacing it
        use crate::settings::protovalidate_extension;
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::String);

        // Cursor right after the dot (offset 5), full expression still present
        let items = get_completions_with_env("this.isEmail()", Position::new(0, 5), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"isEmail"),
            "should suggest isEmail mid-expression: {:?}",
            names
        );
        assert!(
            names.contains(&"contains"),
            "should suggest contains mid-expression: {:?}",
            names
        );
    }

    #[test]
    fn member_access_with_partial_mid_expression() {
        // Cursor after `this.is` in `this.isEmail()` — prefix "is" should filter
        use crate::settings::protovalidate_extension;
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::String);

        let items = get_completions_with_env("this.isEmail()", Position::new(0, 7), &env);
        let names = labels(&items);
        // "is" prefix should match isEmail, isHostname, isIp, etc.
        assert!(
            names.contains(&"isEmail"),
            "should suggest isEmail with prefix 'is': {:?}",
            names
        );
        // Should NOT suggest contains (doesn't start with "is")
        assert!(
            !names.contains(&"contains"),
            "should NOT suggest contains with prefix 'is': {:?}",
            names
        );
    }

    #[test]
    fn member_access_with_prefix_filters() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("name", CelType::String);

        // `name.st` — partial prefix after dot
        let items = get_completions_with_env("name.st", Position::new(0, 7), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"startsWith"),
            "should suggest startsWith: {:?}",
            names
        );
        assert!(
            !names.contains(&"contains"),
            "should NOT suggest contains with prefix 'st': {:?}",
            names
        );
    }

    #[test]
    fn no_operators_in_completions() {
        let items = get_completions("", Position::new(0, 0));
        let names = labels(&items);
        assert!(
            !names.iter().any(|n| n.starts_with('_')),
            "should not contain operator functions: {:?}",
            names
        );
    }

    #[test]
    fn completion_items_have_correct_kinds() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("x", CelType::Int);

        // Typing "x" should suggest variable "x" (prefix match)
        let items = get_completions_with_env("x", Position::new(0, 1), &env);
        let x_item = items.iter().find(|i| i.label == "x");
        assert!(x_item.is_some(), "should find x");
        assert_eq!(x_item.unwrap().kind, Some(CompletionItemKind::VARIABLE));

        // Typing "si" should suggest function "size" (prefix match)
        let items = get_completions_with_env("si", Position::new(0, 2), &env);
        let size_item = items.iter().find(|i| i.label == "size");
        assert!(size_item.is_some(), "should find size");
        assert_eq!(size_item.unwrap().kind, Some(CompletionItemKind::FUNCTION));
    }

    #[test]
    fn member_completion_has_snippets() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("name", CelType::String);

        let items = get_completions_with_env("name.", Position::new(0, 5), &env);
        let contains = items.iter().find(|i| i.label == "contains");
        assert!(contains.is_some(), "should find contains");
        let contains = contains.unwrap();
        assert_eq!(contains.insert_text_format, Some(InsertTextFormat::SNIPPET));
        // contains takes one arg
        assert!(
            contains.insert_text.as_ref().unwrap().contains("${1}"),
            "should have placeholder: {:?}",
            contains.insert_text
        );
    }

    #[test]
    fn incomplete_member_access_trailing_dot() {
        // User has typed "this." — incomplete expression, cursor at end
        use crate::settings::protovalidate_extension;
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::String);

        let items = get_completions_with_env("this.", Position::new(0, 5), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"isEmail"),
            "should suggest isEmail on incomplete 'this.': {:?}",
            names
        );
        assert!(
            names.contains(&"contains"),
            "should suggest contains on incomplete 'this.': {:?}",
            names
        );
    }

    #[test]
    fn protovalidate_string_this_completions() {
        // Simulate protovalidate env where `this` is a string field
        use crate::settings::protovalidate_extension;
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::String)
            .with_variable("rules", CelType::Dyn)
            .with_variable("now", CelType::Timestamp);

        let items = get_completions_with_env("this.", Position::new(0, 5), &env);
        let names = labels(&items);
        eprintln!("protovalidate string completions: {:?}", names);
        assert!(
            names.contains(&"isEmail"),
            "should suggest isEmail for string this: {:?}",
            names
        );
        assert!(
            names.contains(&"isUri"),
            "should suggest isUri for string this: {:?}",
            names
        );
        assert!(
            names.contains(&"isHostname"),
            "should suggest isHostname for string this: {:?}",
            names
        );
        assert!(
            names.contains(&"contains"),
            "should also suggest standard string methods: {:?}",
            names
        );
    }

    #[test]
    fn protovalidate_int_this_no_string_methods() {
        // Simulate protovalidate env where `this` is an int field
        use crate::settings::protovalidate_extension;
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::Int)
            .with_variable("rules", CelType::Dyn)
            .with_variable("now", CelType::Timestamp);

        let items = get_completions_with_env("this.", Position::new(0, 5), &env);
        let names = labels(&items);
        // isEmail should NOT show for int
        assert!(
            !names.contains(&"isEmail"),
            "should NOT suggest isEmail for int this: {:?}",
            names
        );
        // No standard member methods exist for int either
        assert!(
            !names.contains(&"contains"),
            "should NOT suggest string methods for int: {:?}",
            names
        );
    }

    #[test]
    fn protovalidate_double_this_completions() {
        // Simulate protovalidate env where `this` is a double field
        use crate::settings::protovalidate_extension;
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::Double)
            .with_variable("rules", CelType::Dyn)
            .with_variable("now", CelType::Timestamp);

        let items = get_completions_with_env("this.", Position::new(0, 5), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"isNan"),
            "should suggest isNan for double this: {:?}",
            names
        );
        assert!(
            names.contains(&"isInf"),
            "should suggest isInf for double this: {:?}",
            names
        );
    }

    #[test]
    fn protovalidate_list_this_completions() {
        // Simulate protovalidate env where `this` is a repeated field (list)
        use crate::settings::protovalidate_extension;
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::list(CelType::String))
            .with_variable("rules", CelType::Dyn)
            .with_variable("now", CelType::Timestamp);

        let items = get_completions_with_env("this.", Position::new(0, 5), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"unique"),
            "should suggest unique for list this: {:?}",
            names
        );
        assert!(
            names.contains(&"size"),
            "should suggest size for list this: {:?}",
            names
        );
    }

    #[test]
    fn completion_inside_has_macro() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("this", CelType::String);

        // `has(this.)` — cursor after the dot inside has()
        let items = get_completions_with_env("has(this.)", Position::new(0, 9), &env);
        let names = labels(&items);
        assert!(
            names.contains(&"contains"),
            "should suggest string methods inside has(): {:?}",
            names
        );
        assert!(
            names.contains(&"startsWith"),
            "should suggest startsWith inside has(): {:?}",
            names
        );
    }

    #[test]
    fn detect_context_member_access() {
        let ctx = detect_context("foo.", 4);
        assert!(matches!(ctx, CompletionContext::MemberAccess { prefix } if prefix.is_empty()));
    }

    #[test]
    fn detect_context_member_access_with_prefix() {
        let ctx = detect_context("foo.ba", 6);
        assert!(matches!(ctx, CompletionContext::MemberAccess { ref prefix } if prefix == "ba"));
    }

    #[test]
    fn detect_context_identifier() {
        let ctx = detect_context("si", 2);
        assert!(matches!(ctx, CompletionContext::Identifier { ref prefix } if prefix == "si"));
    }

    #[test]
    fn detect_context_empty() {
        let ctx = detect_context("", 0);
        assert!(matches!(ctx, CompletionContext::Identifier { ref prefix } if prefix.is_empty()));
    }
}
