//! Bidirectional converter between cel-parser AST and proto Expr.

use crate::error::ConversionError;
use crate::gen::cel::expr::{
    constant::ConstantKind,
    expr::{
        create_struct::{entry::KeyKind, Entry},
        Call, CreateList, CreateStruct, ExprKind, Ident, Select,
    },
    Constant, ParsedExpr, SourceInfo,
};
use crate::operators::{
    binary_op_to_function, function_to_binary_op, function_to_unary_op, is_index_function,
    is_ternary_function, unary_op_to_function, INDEX_FUNCTION, TERNARY_FUNCTION,
};
use crate::source_info::{build_source_info, compute_line_offsets, get_position};
use cel_core_parser::{Expr, Spanned, SpannedExpr};
use std::collections::HashMap;

/// Bidirectional converter between cel-parser AST and proto Expr.
pub struct AstConverter {
    positions: HashMap<i64, i32>,
    line_offsets: Vec<i32>,
}

impl AstConverter {
    /// Create a new converter for the given source text.
    pub fn new(source: &str) -> Self {
        let line_offsets = compute_line_offsets(source);
        Self {
            positions: HashMap::new(),
            line_offsets,
        }
    }

    /// Convert a cel-parser AST to a proto Expr.
    pub fn ast_to_expr(&mut self, spanned: &SpannedExpr) -> crate::gen::cel::expr::Expr {
        let id = spanned.id;
        // Record position as byte offset from start
        self.positions.insert(id, spanned.span.start as i32);

        let expr_kind = match &spanned.node {
            Expr::Null => Some(ExprKind::ConstExpr(Constant {
                constant_kind: Some(ConstantKind::NullValue(0)),
            })),
            Expr::Bool(v) => Some(ExprKind::ConstExpr(Constant {
                constant_kind: Some(ConstantKind::BoolValue(*v)),
            })),
            Expr::Int(v) => Some(ExprKind::ConstExpr(Constant {
                constant_kind: Some(ConstantKind::Int64Value(*v)),
            })),
            Expr::UInt(v) => Some(ExprKind::ConstExpr(Constant {
                constant_kind: Some(ConstantKind::Uint64Value(*v)),
            })),
            Expr::Float(v) => Some(ExprKind::ConstExpr(Constant {
                constant_kind: Some(ConstantKind::DoubleValue(*v)),
            })),
            Expr::String(v) => Some(ExprKind::ConstExpr(Constant {
                constant_kind: Some(ConstantKind::StringValue(v.clone())),
            })),
            Expr::Bytes(v) => Some(ExprKind::ConstExpr(Constant {
                constant_kind: Some(ConstantKind::BytesValue(v.clone().into())),
            })),
            Expr::Ident(name) => Some(ExprKind::IdentExpr(Ident { name: name.clone() })),
            Expr::RootIdent(name) => {
                // Root ident starts with a select from empty
                Some(ExprKind::SelectExpr(Box::new(Select {
                    operand: None,
                    field: name.clone(),
                    test_only: false,
                })))
            }
            Expr::List(elements) => {
                let converted: Vec<_> = elements.iter().map(|e| self.ast_to_expr(e)).collect();
                Some(ExprKind::ListExpr(CreateList {
                    elements: converted,
                    optional_indices: vec![],
                }))
            }
            Expr::Map(entries) => {
                let converted: Vec<Entry> = entries
                    .iter()
                    .map(|(k, v)| {
                        // Use key's ID for entry ID
                        let entry_id = k.id;
                        self.positions.insert(entry_id, k.span.start as i32);
                        Entry {
                            id: entry_id,
                            key_kind: Some(KeyKind::MapKey(self.ast_to_expr(k))),
                            value: Some(self.ast_to_expr(v)),
                            optional_entry: false,
                        }
                    })
                    .collect();
                Some(ExprKind::StructExpr(CreateStruct {
                    message_name: String::new(),
                    entries: converted,
                }))
            }
            Expr::Unary { op, expr } => {
                let function = unary_op_to_function(*op);
                Some(ExprKind::CallExpr(Box::new(Call {
                    target: None,
                    function: function.to_string(),
                    args: vec![self.ast_to_expr(expr)],
                })))
            }
            Expr::Binary { op, left, right } => {
                let function = binary_op_to_function(*op);
                Some(ExprKind::CallExpr(Box::new(Call {
                    target: None,
                    function: function.to_string(),
                    args: vec![self.ast_to_expr(left), self.ast_to_expr(right)],
                })))
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => Some(ExprKind::CallExpr(Box::new(Call {
                target: None,
                function: TERNARY_FUNCTION.to_string(),
                args: vec![
                    self.ast_to_expr(cond),
                    self.ast_to_expr(then_expr),
                    self.ast_to_expr(else_expr),
                ],
            }))),
            Expr::Member { expr, field } => Some(ExprKind::SelectExpr(Box::new(Select {
                operand: Some(Box::new(self.ast_to_expr(expr))),
                field: field.clone(),
                test_only: false,
            }))),
            Expr::Index { expr, index } => Some(ExprKind::CallExpr(Box::new(Call {
                target: Some(Box::new(self.ast_to_expr(expr))),
                function: INDEX_FUNCTION.to_string(),
                args: vec![self.ast_to_expr(index)],
            }))),
            Expr::Call { expr, args } => {
                // Determine if this is a function call or method call
                let (target, function) = match &expr.node {
                    Expr::Ident(name) => (None, name.clone()),
                    Expr::Member {
                        expr: receiver,
                        field,
                    } => (Some(Box::new(self.ast_to_expr(receiver))), field.clone()),
                    _ => {
                        // Expression call - not typical CEL but handle it
                        (Some(Box::new(self.ast_to_expr(expr))), String::new())
                    }
                };
                let converted_args: Vec<_> = args.iter().map(|a| self.ast_to_expr(a)).collect();
                Some(ExprKind::CallExpr(Box::new(Call {
                    target,
                    function,
                    args: converted_args,
                })))
            }
            Expr::Struct { type_name, fields } => {
                let message_name = extract_type_name(type_name);
                let converted: Vec<Entry> = fields
                    .iter()
                    .map(|(name, value)| {
                        // Use value's ID for entry ID
                        let entry_id = value.id;
                        self.positions.insert(entry_id, value.span.start as i32);
                        Entry {
                            id: entry_id,
                            key_kind: Some(KeyKind::FieldKey(name.clone())),
                            value: Some(self.ast_to_expr(value)),
                            optional_entry: false,
                        }
                    })
                    .collect();
                Some(ExprKind::StructExpr(CreateStruct {
                    message_name,
                    entries: converted,
                }))
            }
            Expr::Error => None,
        };

        crate::gen::cel::expr::Expr { id, expr_kind }
    }

    /// Consume the converter and build a ParsedExpr with source info.
    pub fn into_parsed_expr(self, expr: crate::gen::cel::expr::Expr) -> ParsedExpr {
        ParsedExpr {
            expr: Some(expr),
            source_info: Some(build_source_info(self.positions, self.line_offsets)),
        }
    }

    /// Get the collected source info.
    pub fn into_source_info(self) -> SourceInfo {
        build_source_info(self.positions, self.line_offsets)
    }

    /// Convert a proto Expr back to a cel-parser AST.
    ///
    /// Note: Proto only stores start offsets, so spans will be zero-length
    /// at the recorded position. IDs from the proto are preserved.
    pub fn expr_to_ast(
        &self,
        expr: &crate::gen::cel::expr::Expr,
        source_info: &SourceInfo,
    ) -> Result<SpannedExpr, ConversionError> {
        let pos = get_position(source_info, expr.id).unwrap_or(0);
        let span = pos..pos; // Zero-length span at recorded position
        let id = expr.id; // Preserve ID from proto

        let node = match &expr.expr_kind {
            None => Expr::Error,
            Some(kind) => match kind {
                ExprKind::ConstExpr(c) => self.const_to_ast(expr.id, c)?,
                ExprKind::IdentExpr(ident) => Expr::Ident(ident.name.clone()),
                ExprKind::SelectExpr(select) => {
                    if let Some(operand) = &select.operand {
                        Expr::Member {
                            expr: Box::new(self.expr_to_ast(operand, source_info)?),
                            field: select.field.clone(),
                        }
                    } else {
                        // Select without operand is a root ident
                        Expr::RootIdent(select.field.clone())
                    }
                }
                ExprKind::CallExpr(call) => self.call_to_ast(expr.id, call, source_info)?,
                ExprKind::ListExpr(list) => {
                    let elements: Result<Vec<_>, _> = list
                        .elements
                        .iter()
                        .map(|e| self.expr_to_ast(e, source_info))
                        .collect();
                    Expr::List(elements?)
                }
                ExprKind::StructExpr(s) => self.struct_to_ast(expr.id, s, source_info)?,
                ExprKind::ComprehensionExpr(_) => {
                    return Err(ConversionError::UnsupportedComprehension { expr_id: expr.id });
                }
            },
        };

        Ok(Spanned::new(id, node, span))
    }

    fn const_to_ast(&self, expr_id: i64, c: &Constant) -> Result<Expr, ConversionError> {
        match &c.constant_kind {
            None => Err(ConversionError::MissingField {
                expr_id,
                field: "constant_kind",
            }),
            Some(kind) => match kind {
                ConstantKind::NullValue(_) => Ok(Expr::Null),
                ConstantKind::BoolValue(v) => Ok(Expr::Bool(*v)),
                ConstantKind::Int64Value(v) => Ok(Expr::Int(*v)),
                ConstantKind::Uint64Value(v) => Ok(Expr::UInt(*v)),
                ConstantKind::DoubleValue(v) => Ok(Expr::Float(*v)),
                ConstantKind::StringValue(v) => Ok(Expr::String(v.clone())),
                ConstantKind::BytesValue(v) => Ok(Expr::Bytes(v.to_vec())),
                ConstantKind::DurationValue(_) => Err(ConversionError::InvalidConstant {
                    expr_id,
                    message: "duration constants are not supported".to_string(),
                }),
                ConstantKind::TimestampValue(_) => Err(ConversionError::InvalidConstant {
                    expr_id,
                    message: "timestamp constants are not supported".to_string(),
                }),
            },
        }
    }

    fn call_to_ast(
        &self,
        expr_id: i64,
        call: &Call,
        source_info: &SourceInfo,
    ) -> Result<Expr, ConversionError> {
        let function = &call.function;

        // Check for special operators
        if is_ternary_function(function) {
            if call.args.len() != 3 {
                return Err(ConversionError::InvalidConstant {
                    expr_id,
                    message: format!("ternary requires 3 args, got {}", call.args.len()),
                });
            }
            return Ok(Expr::Ternary {
                cond: Box::new(self.expr_to_ast(&call.args[0], source_info)?),
                then_expr: Box::new(self.expr_to_ast(&call.args[1], source_info)?),
                else_expr: Box::new(self.expr_to_ast(&call.args[2], source_info)?),
            });
        }

        if is_index_function(function) {
            if call.args.len() != 1 {
                return Err(ConversionError::InvalidConstant {
                    expr_id,
                    message: format!("index requires 1 arg, got {}", call.args.len()),
                });
            }
            let target = call.target.as_ref().ok_or(ConversionError::MissingField {
                expr_id,
                field: "target",
            })?;
            return Ok(Expr::Index {
                expr: Box::new(self.expr_to_ast(target, source_info)?),
                index: Box::new(self.expr_to_ast(&call.args[0], source_info)?),
            });
        }

        // Check for unary operators (1 arg, no target)
        if call.target.is_none() && call.args.len() == 1 {
            if let Some(op) = function_to_unary_op(function) {
                return Ok(Expr::Unary {
                    op,
                    expr: Box::new(self.expr_to_ast(&call.args[0], source_info)?),
                });
            }
        }

        // Check for binary operators (2 args, no target)
        if call.target.is_none() && call.args.len() == 2 {
            if let Some(op) = function_to_binary_op(function) {
                return Ok(Expr::Binary {
                    op,
                    left: Box::new(self.expr_to_ast(&call.args[0], source_info)?),
                    right: Box::new(self.expr_to_ast(&call.args[1], source_info)?),
                });
            }
        }

        // Regular function or method call
        let args: Result<Vec<_>, _> = call
            .args
            .iter()
            .map(|a| self.expr_to_ast(a, source_info))
            .collect();
        let args = args?;

        if let Some(target) = &call.target {
            // Method call: target.function(args)
            let receiver = self.expr_to_ast(target, source_info)?;
            let pos = get_position(source_info, expr_id).unwrap_or(0);
            // Use 0 for synthetic nodes created during proto->AST conversion
            let call_expr = Spanned::new(
                0,
                Expr::Member {
                    expr: Box::new(receiver),
                    field: function.clone(),
                },
                pos..pos,
            );
            Ok(Expr::Call {
                expr: Box::new(call_expr),
                args,
            })
        } else {
            // Global function call: function(args)
            let pos = get_position(source_info, expr_id).unwrap_or(0);
            // Use 0 for synthetic nodes created during proto->AST conversion
            let ident_expr = Spanned::new(0, Expr::Ident(function.clone()), pos..pos);
            Ok(Expr::Call {
                expr: Box::new(ident_expr),
                args,
            })
        }
    }

    fn struct_to_ast(
        &self,
        _expr_id: i64,
        s: &CreateStruct,
        source_info: &SourceInfo,
    ) -> Result<Expr, ConversionError> {
        if s.message_name.is_empty() {
            // Map literal
            let entries: Result<Vec<_>, _> = s
                .entries
                .iter()
                .map(|entry| {
                    let key = match &entry.key_kind {
                        Some(KeyKind::MapKey(k)) => self.expr_to_ast(k, source_info)?,
                        _ => {
                            return Err(ConversionError::MissingField {
                                expr_id: entry.id,
                                field: "map_key",
                            })
                        }
                    };
                    let value = entry.value.as_ref().ok_or(ConversionError::MissingField {
                        expr_id: entry.id,
                        field: "value",
                    })?;
                    Ok((key, self.expr_to_ast(value, source_info)?))
                })
                .collect();
            Ok(Expr::Map(entries?))
        } else {
            // Struct literal
            let pos = 0; // We don't have position info for the type name
            let type_name = build_type_name_expr(&s.message_name, pos);

            let fields: Result<Vec<_>, _> = s
                .entries
                .iter()
                .map(|entry| {
                    let field_name = match &entry.key_kind {
                        Some(KeyKind::FieldKey(name)) => name.clone(),
                        _ => {
                            return Err(ConversionError::MissingField {
                                expr_id: entry.id,
                                field: "field_key",
                            })
                        }
                    };
                    let value = entry.value.as_ref().ok_or(ConversionError::MissingField {
                        expr_id: entry.id,
                        field: "value",
                    })?;
                    Ok((field_name, self.expr_to_ast(value, source_info)?))
                })
                .collect();

            Ok(Expr::Struct {
                type_name: Box::new(type_name),
                fields: fields?,
            })
        }
    }
}

/// Extract a dotted type name from a type expression.
fn extract_type_name(expr: &SpannedExpr) -> String {
    match &expr.node {
        Expr::Ident(name) => name.clone(),
        Expr::RootIdent(name) => format!(".{}", name),
        Expr::Member { expr, field } => {
            let prefix = extract_type_name(expr);
            if prefix.is_empty() {
                field.clone()
            } else {
                format!("{}.{}", prefix, field)
            }
        }
        _ => String::new(),
    }
}

/// Build a type name expression from a dotted string.
/// Uses ID 0 for synthetic nodes created during proto->AST conversion.
fn build_type_name_expr(name: &str, pos: usize) -> SpannedExpr {
    let span = pos..pos;

    if let Some(rest) = name.strip_prefix('.') {
        // Root-scoped type
        let parts: Vec<&str> = rest.split('.').collect();
        if parts.len() == 1 {
            Spanned::new(0, Expr::RootIdent(parts[0].to_string()), span)
        } else {
            // .a.b.c -> Member(Member(RootIdent(a), b), c)
            let mut expr = Spanned::new(0, Expr::RootIdent(parts[0].to_string()), span.clone());
            for part in &parts[1..] {
                expr = Spanned::new(
                    0,
                    Expr::Member {
                        expr: Box::new(expr),
                        field: (*part).to_string(),
                    },
                    span.clone(),
                );
            }
            expr
        }
    } else {
        let parts: Vec<&str> = name.split('.').collect();
        if parts.len() == 1 {
            Spanned::new(0, Expr::Ident(parts[0].to_string()), span)
        } else {
            // a.b.c -> Member(Member(Ident(a), b), c)
            let mut expr = Spanned::new(0, Expr::Ident(parts[0].to_string()), span.clone());
            for part in &parts[1..] {
                expr = Spanned::new(
                    0,
                    Expr::Member {
                        expr: Box::new(expr),
                        field: (*part).to_string(),
                    },
                    span.clone(),
                );
            }
            expr
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core_parser::{BinaryOp, UnaryOp};

    fn make_ast(node: Expr) -> SpannedExpr {
        // Use ID 0 for test nodes since exact ID doesn't matter for roundtrip tests
        Spanned::new(0, node, 0..1)
    }

    fn roundtrip(ast: &SpannedExpr) -> SpannedExpr {
        let source = "";
        let mut converter = AstConverter::new(source);
        let proto_expr = converter.ast_to_expr(ast);
        let source_info = converter.into_source_info();
        let new_converter = AstConverter::new(source);
        new_converter.expr_to_ast(&proto_expr, &source_info).unwrap()
    }

    fn assert_node_eq(a: &Expr, b: &Expr) {
        // Compare nodes without spans
        match (a, b) {
            (Expr::Null, Expr::Null) => {}
            (Expr::Bool(a), Expr::Bool(b)) => assert_eq!(a, b),
            (Expr::Int(a), Expr::Int(b)) => assert_eq!(a, b),
            (Expr::UInt(a), Expr::UInt(b)) => assert_eq!(a, b),
            (Expr::Float(a), Expr::Float(b)) => assert!((a - b).abs() < f64::EPSILON),
            (Expr::String(a), Expr::String(b)) => assert_eq!(a, b),
            (Expr::Bytes(a), Expr::Bytes(b)) => assert_eq!(a, b),
            (Expr::Ident(a), Expr::Ident(b)) => assert_eq!(a, b),
            (Expr::RootIdent(a), Expr::RootIdent(b)) => assert_eq!(a, b),
            (Expr::List(a), Expr::List(b)) => {
                assert_eq!(a.len(), b.len());
                for (x, y) in a.iter().zip(b.iter()) {
                    assert_node_eq(&x.node, &y.node);
                }
            }
            (Expr::Map(a), Expr::Map(b)) => {
                assert_eq!(a.len(), b.len());
                for ((k1, v1), (k2, v2)) in a.iter().zip(b.iter()) {
                    assert_node_eq(&k1.node, &k2.node);
                    assert_node_eq(&v1.node, &v2.node);
                }
            }
            (
                Expr::Unary { op: op1, expr: e1 },
                Expr::Unary { op: op2, expr: e2 },
            ) => {
                assert_eq!(op1, op2);
                assert_node_eq(&e1.node, &e2.node);
            }
            (
                Expr::Binary {
                    op: op1,
                    left: l1,
                    right: r1,
                },
                Expr::Binary {
                    op: op2,
                    left: l2,
                    right: r2,
                },
            ) => {
                assert_eq!(op1, op2);
                assert_node_eq(&l1.node, &l2.node);
                assert_node_eq(&r1.node, &r2.node);
            }
            (
                Expr::Ternary {
                    cond: c1,
                    then_expr: t1,
                    else_expr: e1,
                },
                Expr::Ternary {
                    cond: c2,
                    then_expr: t2,
                    else_expr: e2,
                },
            ) => {
                assert_node_eq(&c1.node, &c2.node);
                assert_node_eq(&t1.node, &t2.node);
                assert_node_eq(&e1.node, &e2.node);
            }
            (
                Expr::Member { expr: e1, field: f1 },
                Expr::Member { expr: e2, field: f2 },
            ) => {
                assert_eq!(f1, f2);
                assert_node_eq(&e1.node, &e2.node);
            }
            (
                Expr::Index { expr: e1, index: i1 },
                Expr::Index { expr: e2, index: i2 },
            ) => {
                assert_node_eq(&e1.node, &e2.node);
                assert_node_eq(&i1.node, &i2.node);
            }
            (
                Expr::Call { expr: e1, args: a1 },
                Expr::Call { expr: e2, args: a2 },
            ) => {
                assert_node_eq(&e1.node, &e2.node);
                assert_eq!(a1.len(), a2.len());
                for (x, y) in a1.iter().zip(a2.iter()) {
                    assert_node_eq(&x.node, &y.node);
                }
            }
            (
                Expr::Struct {
                    type_name: t1,
                    fields: f1,
                },
                Expr::Struct {
                    type_name: t2,
                    fields: f2,
                },
            ) => {
                assert_node_eq(&t1.node, &t2.node);
                assert_eq!(f1.len(), f2.len());
                for ((n1, v1), (n2, v2)) in f1.iter().zip(f2.iter()) {
                    assert_eq!(n1, n2);
                    assert_node_eq(&v1.node, &v2.node);
                }
            }
            (Expr::Error, Expr::Error) => {}
            _ => panic!("nodes don't match: {:?} vs {:?}", a, b),
        }
    }

    #[test]
    fn test_roundtrip_null() {
        let ast = make_ast(Expr::Null);
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_bool() {
        let ast = make_ast(Expr::Bool(true));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_int() {
        let ast = make_ast(Expr::Int(42));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_uint() {
        let ast = make_ast(Expr::UInt(42));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_float() {
        let ast = make_ast(Expr::Float(3.14));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_string() {
        let ast = make_ast(Expr::String("hello".to_string()));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_bytes() {
        let ast = make_ast(Expr::Bytes(vec![1, 2, 3]));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_ident() {
        let ast = make_ast(Expr::Ident("x".to_string()));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_root_ident() {
        let ast = make_ast(Expr::RootIdent("x".to_string()));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_list() {
        let ast = make_ast(Expr::List(vec![
            make_ast(Expr::Int(1)),
            make_ast(Expr::Int(2)),
        ]));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_map() {
        let ast = make_ast(Expr::Map(vec![(
            make_ast(Expr::String("key".to_string())),
            make_ast(Expr::Int(42)),
        )]));
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_unary_neg() {
        let ast = make_ast(Expr::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(make_ast(Expr::Int(5))),
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_unary_not() {
        let ast = make_ast(Expr::Unary {
            op: UnaryOp::Not,
            expr: Box::new(make_ast(Expr::Bool(true))),
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_binary_add() {
        let ast = make_ast(Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(make_ast(Expr::Int(1))),
            right: Box::new(make_ast(Expr::Int(2))),
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_all_binary_ops() {
        let ops = [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Mod,
            BinaryOp::Eq,
            BinaryOp::Ne,
            BinaryOp::Lt,
            BinaryOp::Le,
            BinaryOp::Gt,
            BinaryOp::Ge,
            BinaryOp::In,
            BinaryOp::And,
            BinaryOp::Or,
        ];
        for op in ops {
            let ast = make_ast(Expr::Binary {
                op,
                left: Box::new(make_ast(Expr::Int(1))),
                right: Box::new(make_ast(Expr::Int(2))),
            });
            let result = roundtrip(&ast);
            assert_node_eq(&ast.node, &result.node);
        }
    }

    #[test]
    fn test_roundtrip_ternary() {
        let ast = make_ast(Expr::Ternary {
            cond: Box::new(make_ast(Expr::Bool(true))),
            then_expr: Box::new(make_ast(Expr::Int(1))),
            else_expr: Box::new(make_ast(Expr::Int(2))),
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_member() {
        let ast = make_ast(Expr::Member {
            expr: Box::new(make_ast(Expr::Ident("x".to_string()))),
            field: "y".to_string(),
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_index() {
        let ast = make_ast(Expr::Index {
            expr: Box::new(make_ast(Expr::Ident("x".to_string()))),
            index: Box::new(make_ast(Expr::Int(0))),
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_function_call() {
        let ast = make_ast(Expr::Call {
            expr: Box::new(make_ast(Expr::Ident("size".to_string()))),
            args: vec![make_ast(Expr::Ident("x".to_string()))],
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_method_call() {
        let ast = make_ast(Expr::Call {
            expr: Box::new(make_ast(Expr::Member {
                expr: Box::new(make_ast(Expr::Ident("x".to_string()))),
                field: "size".to_string(),
            })),
            args: vec![],
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_roundtrip_struct() {
        let ast = make_ast(Expr::Struct {
            type_name: Box::new(make_ast(Expr::Ident("MyType".to_string()))),
            fields: vec![("field".to_string(), make_ast(Expr::Int(42)))],
        });
        let result = roundtrip(&ast);
        assert_node_eq(&ast.node, &result.node);
    }

    #[test]
    fn test_extract_type_name() {
        // Simple identifier
        let expr = make_ast(Expr::Ident("Foo".to_string()));
        assert_eq!(extract_type_name(&expr), "Foo");

        // Root identifier
        let expr = make_ast(Expr::RootIdent("Foo".to_string()));
        assert_eq!(extract_type_name(&expr), ".Foo");

        // Member chain
        let expr = make_ast(Expr::Member {
            expr: Box::new(make_ast(Expr::Member {
                expr: Box::new(make_ast(Expr::Ident("a".to_string()))),
                field: "b".to_string(),
            })),
            field: "c".to_string(),
        });
        assert_eq!(extract_type_name(&expr), "a.b.c");
    }

    #[test]
    fn test_build_type_name_expr() {
        // Simple name
        let expr = build_type_name_expr("Foo", 0);
        assert!(matches!(expr.node, Expr::Ident(ref s) if s == "Foo"));

        // Dotted name
        let expr = build_type_name_expr("a.b.c", 0);
        assert!(matches!(expr.node, Expr::Member { .. }));

        // Root-scoped name
        let expr = build_type_name_expr(".Foo", 0);
        assert!(matches!(expr.node, Expr::RootIdent(ref s) if s == "Foo"));

        // Root-scoped dotted name
        let expr = build_type_name_expr(".a.b", 0);
        assert!(matches!(expr.node, Expr::Member { .. }));
    }
}
