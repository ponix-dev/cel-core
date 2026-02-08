//! Semantic tokens for CEL syntax highlighting.

use cel_core::{
    types::{BinaryOp, Expr, UnaryOp},
    SpannedExpr,
};
use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend,
};

use crate::document::{LineIndex, ProtoDocumentState};
use crate::types::is_builtin;

/// Token type indices (must match LEGEND order).
pub mod token_types {
    pub const KEYWORD: u32 = 0;
    pub const NUMBER: u32 = 1;
    pub const STRING: u32 = 2;
    pub const OPERATOR: u32 = 3;
    pub const VARIABLE: u32 = 4;
    pub const FUNCTION: u32 = 5;
    pub const METHOD: u32 = 6;
    pub const PUNCTUATION: u32 = 7;
}

/// Token modifier bit flags.
pub mod token_modifiers {
    pub const DEFAULT_LIBRARY: u32 = 1 << 0;
}

/// Get the semantic tokens legend for capability declaration.
pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::KEYWORD,
            SemanticTokenType::NUMBER,
            SemanticTokenType::STRING,
            SemanticTokenType::OPERATOR,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::METHOD,
            SemanticTokenType::new("punctuation"),
        ],
        token_modifiers: vec![SemanticTokenModifier::DEFAULT_LIBRARY],
    }
}

/// A raw token before delta encoding.
#[derive(Debug, Clone)]
struct RawToken {
    start: usize,
    length: usize,
    token_type: u32,
    token_modifiers: u32,
}

/// Collector for semantic tokens.
struct TokenCollector<'a> {
    source: &'a str,
    tokens: Vec<RawToken>,
}

impl<'a> TokenCollector<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
        }
    }

    fn push(&mut self, start: usize, end: usize, token_type: u32, token_modifiers: u32) {
        if start < end && end <= self.source.len() {
            self.tokens.push(RawToken {
                start,
                length: end - start,
                token_type,
                token_modifiers,
            });
        }
    }

    fn push_punctuation(&mut self, start: usize, len: usize) {
        self.push(start, start + len, token_types::PUNCTUATION, 0);
    }

    /// Find a single character in the source between start and end.
    fn find_char(&self, start: usize, end: usize, c: char) -> Option<usize> {
        self.source[start..end].find(c).map(|i| start + i)
    }

    fn visit_expr(&mut self, expr: &SpannedExpr) {
        match &expr.node {
            Expr::Null => {
                self.push(expr.span.start, expr.span.end, token_types::KEYWORD, 0);
            }
            Expr::Bool(_) => {
                self.push(expr.span.start, expr.span.end, token_types::KEYWORD, 0);
            }
            Expr::Int(_) | Expr::UInt(_) | Expr::Float(_) => {
                self.push(expr.span.start, expr.span.end, token_types::NUMBER, 0);
            }
            Expr::String(_) | Expr::Bytes(_) => {
                self.push(expr.span.start, expr.span.end, token_types::STRING, 0);
            }
            Expr::Ident(name) | Expr::RootIdent(name) => {
                let modifiers = if is_builtin(name) {
                    token_modifiers::DEFAULT_LIBRARY
                } else {
                    0
                };
                self.push(
                    expr.span.start,
                    expr.span.end,
                    token_types::VARIABLE,
                    modifiers,
                );
            }
            Expr::List(items) => {
                // Opening bracket
                self.push_punctuation(expr.span.start, 1);

                for item in items {
                    self.visit_expr(&item.expr);
                }

                // Commas between items
                for window in items.windows(2) {
                    let gap_start = window[0].expr.span.end;
                    let gap_end = window[1].expr.span.start;
                    if let Some(pos) = self.find_char(gap_start, gap_end, ',') {
                        self.push_punctuation(pos, 1);
                    }
                }

                // Closing bracket
                self.push_punctuation(expr.span.end - 1, 1);
            }
            Expr::Map(entries) => {
                // Opening brace
                self.push_punctuation(expr.span.start, 1);

                for entry in entries {
                    self.visit_expr(&entry.key);

                    // Colon between key and value
                    let gap_start = entry.key.span.end;
                    let gap_end = entry.value.span.start;
                    if let Some(pos) = self.find_char(gap_start, gap_end, ':') {
                        self.push_punctuation(pos, 1);
                    }

                    self.visit_expr(&entry.value);
                }

                // Commas between entries
                for window in entries.windows(2) {
                    let gap_start = window[0].value.span.end;
                    let gap_end = window[1].key.span.start;
                    if let Some(pos) = self.find_char(gap_start, gap_end, ',') {
                        self.push_punctuation(pos, 1);
                    }
                }

                // Closing brace
                self.push_punctuation(expr.span.end - 1, 1);
            }
            Expr::Unary { op, expr: inner } => {
                let op_len = match op {
                    UnaryOp::Neg | UnaryOp::Not => 1,
                };
                self.push(
                    expr.span.start,
                    expr.span.start + op_len,
                    token_types::OPERATOR,
                    0,
                );
                self.visit_expr(inner);
            }
            Expr::Binary { op, left, right } => {
                self.visit_expr(left);

                let op_start = left.span.end;
                let op_end = right.span.start;
                if let Some((op_text, op_offset)) = self.find_operator(op_start, op_end, *op) {
                    self.push(
                        op_start + op_offset,
                        op_start + op_offset + op_text.len(),
                        token_types::OPERATOR,
                        0,
                    );
                }

                self.visit_expr(right);
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.visit_expr(cond);

                // Question mark
                let gap1_start = cond.span.end;
                let gap1_end = then_expr.span.start;
                if let Some(pos) = self.find_char(gap1_start, gap1_end, '?') {
                    self.push_punctuation(pos, 1);
                }

                self.visit_expr(then_expr);

                // Colon
                let gap2_start = then_expr.span.end;
                let gap2_end = else_expr.span.start;
                if let Some(pos) = self.find_char(gap2_start, gap2_end, ':') {
                    self.push_punctuation(pos, 1);
                }

                self.visit_expr(else_expr);
            }
            Expr::Member {
                expr: inner, field, ..
            } => {
                self.visit_expr(inner);

                // Dot
                let dot_pos = inner.span.end;
                if dot_pos < expr.span.end {
                    self.push_punctuation(dot_pos, 1);
                }

                // Field
                let field_start = expr.span.end - field.len();
                self.push(field_start, expr.span.end, token_types::VARIABLE, 0);
            }
            Expr::Index {
                expr: inner, index, ..
            } => {
                self.visit_expr(inner);

                // Opening bracket - find it after the inner expression
                if let Some(pos) = self.find_char(inner.span.end, index.span.start, '[') {
                    self.push_punctuation(pos, 1);
                }

                self.visit_expr(index);

                // Closing bracket
                self.push_punctuation(expr.span.end - 1, 1);
            }
            Expr::Call { expr: callee, args } => {
                match &callee.node {
                    Expr::Ident(name) => {
                        let modifiers = if is_builtin(name) {
                            token_modifiers::DEFAULT_LIBRARY
                        } else {
                            0
                        };
                        self.push(
                            callee.span.start,
                            callee.span.end,
                            token_types::FUNCTION,
                            modifiers,
                        );
                    }
                    Expr::Member {
                        expr: obj, field, ..
                    } => {
                        self.visit_expr(obj);

                        // Dot
                        let dot_pos = obj.span.end;
                        if dot_pos < callee.span.end {
                            self.push_punctuation(dot_pos, 1);
                        }

                        let field_start = callee.span.end - field.len();
                        let modifiers = if is_builtin(field) {
                            token_modifiers::DEFAULT_LIBRARY
                        } else {
                            0
                        };
                        self.push(field_start, callee.span.end, token_types::METHOD, modifiers);
                    }
                    _ => {
                        self.visit_expr(callee);
                    }
                }

                // Opening parenthesis
                if let Some(pos) = self.find_char(callee.span.end, expr.span.end, '(') {
                    self.push_punctuation(pos, 1);
                }

                for arg in args {
                    self.visit_expr(arg);
                }

                // Commas between arguments
                for window in args.windows(2) {
                    let gap_start = window[0].span.end;
                    let gap_end = window[1].span.start;
                    if let Some(pos) = self.find_char(gap_start, gap_end, ',') {
                        self.push_punctuation(pos, 1);
                    }
                }

                // Closing parenthesis
                self.push_punctuation(expr.span.end - 1, 1);
            }
            Expr::Struct { type_name, fields } => {
                // Type name
                self.visit_expr(type_name);

                // Opening brace
                if let Some(pos) = self.find_char(type_name.span.end, expr.span.end, '{') {
                    self.push_punctuation(pos, 1);
                }

                for field in fields {
                    // Field name - find it before the colon
                    // We need to locate the field name in the source
                    let field_end = field.value.span.start;
                    if let Some(colon_pos) = self.find_char(type_name.span.end, field_end, ':') {
                        // Field name is just before the colon (with possible whitespace)
                        let field_name_end = colon_pos;
                        let field_name_start = field_name_end.saturating_sub(field.name.len());
                        self.push(field_name_start, field_name_end, token_types::VARIABLE, 0);
                        // Colon
                        self.push_punctuation(colon_pos, 1);
                    }

                    self.visit_expr(&field.value);
                }

                // Commas between fields
                for window in fields.windows(2) {
                    let gap_start = window[0].value.span.end;
                    let gap_end = window[1].value.span.start;
                    if let Some(pos) = self.find_char(gap_start, gap_end, ',') {
                        self.push_punctuation(pos, 1);
                    }
                }

                // Closing brace
                self.push_punctuation(expr.span.end - 1, 1);
            }
            Expr::Comprehension(comp) => {
                // Comprehensions are synthetic - visit sub-expressions
                // Note: The original macro call is in source_info.macro_calls for IDE display
                self.visit_expr(&comp.iter_range);
                self.visit_expr(&comp.accu_init);
                self.visit_expr(&comp.loop_condition);
                self.visit_expr(&comp.loop_step);
                self.visit_expr(&comp.result);
            }
            Expr::MemberTestOnly { expr: inner, field } => {
                // MemberTestOnly is the expansion of has(expr.field)
                // The outer span covers the full `has(expr.field)` source text

                // "has" keyword
                let has_end = expr.span.start + 3;
                self.push(
                    expr.span.start,
                    has_end,
                    token_types::FUNCTION,
                    token_modifiers::DEFAULT_LIBRARY,
                );

                // Opening parenthesis
                if let Some(pos) = self.find_char(has_end, inner.span.start, '(') {
                    self.push_punctuation(pos, 1);
                }

                // Inner expression (e.g., `this` or `msg`)
                self.visit_expr(inner);

                // Dot between inner expression and field
                if let Some(pos) = self.find_char(inner.span.end, expr.span.end, '.') {
                    self.push_punctuation(pos, 1);
                }

                // Field name
                let field_start = expr.span.end - 1 - field.len();
                self.push(
                    field_start,
                    field_start + field.len(),
                    token_types::VARIABLE,
                    0,
                );

                // Closing parenthesis
                self.push_punctuation(expr.span.end - 1, 1);
            }
            Expr::Bind { init, body, .. } => {
                // Bind is synthetic from cel.bind() - visit sub-expressions
                self.visit_expr(init);
                self.visit_expr(body);
            }
            Expr::Error => {
                // Skip error nodes
            }
        }
    }

    fn find_operator(
        &self,
        start: usize,
        end: usize,
        op: BinaryOp,
    ) -> Option<(&'static str, usize)> {
        let op_str = match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::In => "in",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        };

        let slice = &self.source[start..end];
        slice.find(op_str).map(|offset| (op_str, offset))
    }

    fn into_semantic_tokens(mut self, line_index: &LineIndex) -> Vec<SemanticToken> {
        // Sort by position
        self.tokens.sort_by_key(|t| t.start);

        let mut result = Vec::with_capacity(self.tokens.len());
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;

        for token in &self.tokens {
            let pos = line_index.offset_to_position(token.start);
            let delta_line = pos.line - prev_line;
            let delta_start = if delta_line == 0 {
                pos.character - prev_start
            } else {
                pos.character
            };

            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: token.length as u32,
                token_type: token.token_type,
                token_modifiers_bitset: token.token_modifiers,
            });

            prev_line = pos.line;
            prev_start = pos.character;
        }

        result
    }
}

/// Generate semantic tokens for a parsed expression.
pub fn tokens_for_ast(line_index: &LineIndex, ast: &SpannedExpr) -> Vec<SemanticToken> {
    let mut collector = TokenCollector::new(line_index.source());
    collector.visit_expr(ast);
    collector.into_semantic_tokens(line_index)
}

/// Generate semantic tokens for a proto document containing CEL regions.
///
/// This processes all CEL regions, generates tokens for each, and maps
/// them to host document coordinates.
pub fn tokens_for_proto(state: &ProtoDocumentState) -> Vec<SemanticToken> {
    let mut all_tokens: Vec<RawToken> = Vec::new();

    for region_state in &state.regions {
        if let Some(ast) = &region_state.ast {
            // Generate tokens with CEL-local offsets
            let mut collector = TokenCollector::new(&region_state.region.source);
            collector.visit_expr(ast);

            // Convert to host coordinates
            for token in collector.tokens {
                let host_start = region_state.mapper.to_host(token.start);
                all_tokens.push(RawToken {
                    start: host_start,
                    length: token.length,
                    token_type: token.token_type,
                    token_modifiers: token.token_modifiers,
                });
            }
        }
    }

    // Sort by position and convert to delta-encoded format
    all_tokens.sort_by_key(|t| t.start);
    encode_tokens(&all_tokens, &state.line_index)
}

/// Convert raw tokens to delta-encoded semantic tokens.
fn encode_tokens(tokens: &[RawToken], line_index: &LineIndex) -> Vec<SemanticToken> {
    let mut result = Vec::with_capacity(tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for token in tokens {
        let pos = line_index.offset_to_position(token.start);
        let delta_line = pos.line - prev_line;
        let delta_start = if delta_line == 0 {
            pos.character - prev_start
        } else {
            pos.character
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length: token.length as u32,
            token_type: token.token_type,
            token_modifiers_bitset: token.token_modifiers,
        });

        prev_line = pos.line;
        prev_start = pos.character;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core::parse;

    #[test]
    fn legend_has_expected_types() {
        let leg = legend();
        assert!(leg.token_types.contains(&SemanticTokenType::KEYWORD));
        assert!(leg.token_types.contains(&SemanticTokenType::NUMBER));
        assert!(leg.token_types.contains(&SemanticTokenType::FUNCTION));
        assert_eq!(leg.token_types.len(), 8); // Now includes punctuation
    }

    #[test]
    fn tokens_for_simple_expression() {
        let source = "1 + 2";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let tokens = tokens_for_ast(&line_index, &ast);
        // Should have: number(1), operator(+), number(2)
        assert_eq!(tokens.len(), 3);
    }

    #[test]
    fn tokens_for_function_call() {
        let source = "size(x)";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let tokens = tokens_for_ast(&line_index, &ast);
        // Should have: function(size), (, variable(x), )
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].token_type, token_types::FUNCTION);
        assert_eq!(
            tokens[0].token_modifiers_bitset,
            token_modifiers::DEFAULT_LIBRARY
        );
    }

    #[test]
    fn tokens_for_list() {
        let source = "[1, 2]";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let tokens = tokens_for_ast(&line_index, &ast);
        // Should have: [, number(1), comma, number(2), ]
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].token_type, token_types::PUNCTUATION); // [
        assert_eq!(tokens[4].token_type, token_types::PUNCTUATION); // ]
    }

    #[test]
    fn tokens_for_ternary() {
        let source = "a ? b : c";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let tokens = tokens_for_ast(&line_index, &ast);
        // Should have: variable(a), ?, variable(b), :, variable(c)
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[1].token_type, token_types::PUNCTUATION); // ?
        assert_eq!(tokens[3].token_type, token_types::PUNCTUATION); // :
    }

    #[test]
    fn tokens_for_has_macro() {
        let source = "has(msg.field)";
        let result = parse(source);
        let ast = result.ast.unwrap();
        let line_index = LineIndex::new(source.to_string());

        let tokens = tokens_for_ast(&line_index, &ast);
        // Should have: function(has), (, variable(msg), ., variable(field), )
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].token_type, token_types::FUNCTION); // has
        assert_eq!(
            tokens[0].token_modifiers_bitset,
            token_modifiers::DEFAULT_LIBRARY
        );
        assert_eq!(tokens[1].token_type, token_types::PUNCTUATION); // (
        assert_eq!(tokens[2].token_type, token_types::VARIABLE); // msg
        assert_eq!(tokens[3].token_type, token_types::PUNCTUATION); // .
        assert_eq!(tokens[4].token_type, token_types::VARIABLE); // field
        assert_eq!(tokens[5].token_type, token_types::PUNCTUATION); // )
    }
}
