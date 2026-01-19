//! Integration tests for the public parse() API.

mod common;

use cel_core_parser::{BinaryOp, Expr, UnaryOp};

// ============================================================================
// Literal parsing tests
// ============================================================================

#[test]
fn parse_integer_literals() {
    assert_eq!(common::assert_parses("0").node, Expr::Int(0));
    assert_eq!(common::assert_parses("123").node, Expr::Int(123));
    assert_eq!(common::assert_parses("0x1F").node, Expr::Int(31));
    assert_eq!(common::assert_parses("0XAB").node, Expr::Int(171));
}

#[test]
fn parse_unsigned_integer_literals() {
    assert_eq!(common::assert_parses("123u").node, Expr::UInt(123));
    assert_eq!(common::assert_parses("123U").node, Expr::UInt(123));
    assert_eq!(common::assert_parses("0x1Fu").node, Expr::UInt(31));
}

#[test]
fn parse_float_literals() {
    assert_eq!(common::assert_parses("1.5").node, Expr::Float(1.5));
    assert_eq!(common::assert_parses("1e10").node, Expr::Float(1e10));
    assert_eq!(common::assert_parses("1.5e-3").node, Expr::Float(1.5e-3));
}

#[test]
fn parse_string_literals() {
    assert_eq!(
        common::assert_parses(r#""hello""#).node,
        Expr::String("hello".to_string())
    );
    assert_eq!(
        common::assert_parses("'world'").node,
        Expr::String("world".to_string())
    );
}

#[test]
fn parse_string_escapes() {
    assert_eq!(
        common::assert_parses(r#""hello\nworld""#).node,
        Expr::String("hello\nworld".to_string())
    );
    assert_eq!(
        common::assert_parses(r#""tab\there""#).node,
        Expr::String("tab\there".to_string())
    );
}

#[test]
fn parse_raw_strings() {
    assert_eq!(
        common::assert_parses(r#"r"hello\n""#).node,
        Expr::String(r"hello\n".to_string())
    );
}

#[test]
fn parse_bytes_literals() {
    assert_eq!(
        common::assert_parses(r#"b"hello""#).node,
        Expr::Bytes(b"hello".to_vec())
    );
    assert_eq!(
        common::assert_parses("b'world'").node,
        Expr::Bytes(b"world".to_vec())
    );
}

#[test]
fn parse_boolean_literals() {
    assert_eq!(common::assert_parses("true").node, Expr::Bool(true));
    assert_eq!(common::assert_parses("false").node, Expr::Bool(false));
}

#[test]
fn parse_null_literal() {
    assert_eq!(common::assert_parses("null").node, Expr::Null);
}

// ============================================================================
// Identifier tests
// ============================================================================

#[test]
fn parse_identifiers() {
    assert_eq!(
        common::assert_parses("foo").node,
        Expr::Ident("foo".to_string())
    );
    assert_eq!(
        common::assert_parses("_bar").node,
        Expr::Ident("_bar".to_string())
    );
    assert_eq!(
        common::assert_parses("baz123").node,
        Expr::Ident("baz123".to_string())
    );
}

// ============================================================================
// Collection literal tests
// ============================================================================

#[test]
fn parse_empty_list() {
    if let Expr::List(items) = common::assert_parses("[]").node {
        assert!(items.is_empty());
    } else {
        panic!("expected list");
    }
}

#[test]
fn parse_list_literals() {
    if let Expr::List(items) = common::assert_parses("[1, 2, 3]").node {
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].node, Expr::Int(1));
        assert_eq!(items[1].node, Expr::Int(2));
        assert_eq!(items[2].node, Expr::Int(3));
    } else {
        panic!("expected list");
    }
}

#[test]
fn parse_list_with_trailing_comma() {
    if let Expr::List(items) = common::assert_parses("[1, 2,]").node {
        assert_eq!(items.len(), 2);
    } else {
        panic!("expected list");
    }
}

#[test]
fn parse_empty_map() {
    if let Expr::Map(entries) = common::assert_parses("{}").node {
        assert!(entries.is_empty());
    } else {
        panic!("expected map");
    }
}

#[test]
fn parse_map_literals() {
    if let Expr::Map(entries) = common::assert_parses(r#"{"a": 1, "b": 2}"#).node {
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].0.node, Expr::String("a".to_string()));
        assert_eq!(entries[0].1.node, Expr::Int(1));
        assert_eq!(entries[1].0.node, Expr::String("b".to_string()));
        assert_eq!(entries[1].1.node, Expr::Int(2));
    } else {
        panic!("expected map");
    }
}

// ============================================================================
// Operator precedence tests
// ============================================================================

#[test]
fn parse_addition() {
    if let Expr::Binary { op, left, right } = common::assert_parses("1 + 2").node {
        assert_eq!(op, BinaryOp::Add);
        assert_eq!(left.node, Expr::Int(1));
        assert_eq!(right.node, Expr::Int(2));
    } else {
        panic!("expected binary");
    }
}

#[test]
fn parse_multiplication_higher_than_addition() {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    if let Expr::Binary { op, left, right } = common::assert_parses("1 + 2 * 3").node {
        assert_eq!(op, BinaryOp::Add);
        assert_eq!(left.node, Expr::Int(1));
        if let Expr::Binary {
            op: inner_op,
            left: inner_left,
            right: inner_right,
        } = &right.node
        {
            assert_eq!(*inner_op, BinaryOp::Mul);
            assert_eq!(inner_left.node, Expr::Int(2));
            assert_eq!(inner_right.node, Expr::Int(3));
        } else {
            panic!("expected inner multiplication");
        }
    } else {
        panic!("expected addition at top level");
    }
}

#[test]
fn parse_left_associative_subtraction() {
    // 1 - 2 - 3 should parse as (1 - 2) - 3
    if let Expr::Binary { op, left, right } = common::assert_parses("1 - 2 - 3").node {
        assert_eq!(op, BinaryOp::Sub);
        assert_eq!(right.node, Expr::Int(3));
        if let Expr::Binary {
            op: inner_op,
            left: inner_left,
            right: inner_right,
        } = &left.node
        {
            assert_eq!(*inner_op, BinaryOp::Sub);
            assert_eq!(inner_left.node, Expr::Int(1));
            assert_eq!(inner_right.node, Expr::Int(2));
        } else {
            panic!("expected inner subtraction");
        }
    } else {
        panic!("expected subtraction at top level");
    }
}

#[test]
fn parse_comparison_operators() {
    assert!(matches!(
        common::assert_parses("a == b").node,
        Expr::Binary {
            op: BinaryOp::Eq,
            ..
        }
    ));
    assert!(matches!(
        common::assert_parses("a != b").node,
        Expr::Binary {
            op: BinaryOp::Ne,
            ..
        }
    ));
    assert!(matches!(
        common::assert_parses("a < b").node,
        Expr::Binary {
            op: BinaryOp::Lt,
            ..
        }
    ));
    assert!(matches!(
        common::assert_parses("a <= b").node,
        Expr::Binary {
            op: BinaryOp::Le,
            ..
        }
    ));
    assert!(matches!(
        common::assert_parses("a > b").node,
        Expr::Binary {
            op: BinaryOp::Gt,
            ..
        }
    ));
    assert!(matches!(
        common::assert_parses("a >= b").node,
        Expr::Binary {
            op: BinaryOp::Ge,
            ..
        }
    ));
}

#[test]
fn parse_in_operator() {
    if let Expr::Binary { op, left, right } = common::assert_parses("x in list").node {
        assert_eq!(op, BinaryOp::In);
        assert_eq!(left.node, Expr::Ident("x".to_string()));
        assert_eq!(right.node, Expr::Ident("list".to_string()));
    } else {
        panic!("expected binary in");
    }
}

#[test]
fn parse_logical_and() {
    if let Expr::Binary { op, .. } = common::assert_parses("a && b").node {
        assert_eq!(op, BinaryOp::And);
    } else {
        panic!("expected binary and");
    }
}

#[test]
fn parse_logical_or() {
    if let Expr::Binary { op, .. } = common::assert_parses("a || b").node {
        assert_eq!(op, BinaryOp::Or);
    } else {
        panic!("expected binary or");
    }
}

#[test]
fn parse_and_higher_than_or() {
    // a || b && c should parse as a || (b && c)
    if let Expr::Binary { op, left, right } = common::assert_parses("a || b && c").node {
        assert_eq!(op, BinaryOp::Or);
        assert_eq!(left.node, Expr::Ident("a".to_string()));
        if let Expr::Binary { op: inner_op, .. } = &right.node {
            assert_eq!(*inner_op, BinaryOp::And);
        } else {
            panic!("expected inner and");
        }
    } else {
        panic!("expected or at top level");
    }
}

// ============================================================================
// Unary operator tests
// ============================================================================

#[test]
fn parse_unary_negation() {
    if let Expr::Unary { op, expr } = common::assert_parses("-x").node {
        assert_eq!(op, UnaryOp::Neg);
        assert_eq!(expr.node, Expr::Ident("x".to_string()));
    } else {
        panic!("expected unary negation");
    }
}

#[test]
fn parse_unary_not() {
    if let Expr::Unary { op, expr } = common::assert_parses("!x").node {
        assert_eq!(op, UnaryOp::Not);
        assert_eq!(expr.node, Expr::Ident("x".to_string()));
    } else {
        panic!("expected unary not");
    }
}

#[test]
fn parse_double_negation() {
    // --x should parse as -(-x)
    if let Expr::Unary { op, expr } = common::assert_parses("--x").node {
        assert_eq!(op, UnaryOp::Neg);
        if let Expr::Unary {
            op: inner_op,
            expr: inner_expr,
        } = &expr.node
        {
            assert_eq!(*inner_op, UnaryOp::Neg);
            assert_eq!(inner_expr.node, Expr::Ident("x".to_string()));
        } else {
            panic!("expected inner negation");
        }
    } else {
        panic!("expected outer negation");
    }
}

// ============================================================================
// Member access tests
// ============================================================================

#[test]
fn parse_member_access() {
    if let Expr::Member { expr, field } = common::assert_parses("a.b").node {
        assert_eq!(expr.node, Expr::Ident("a".to_string()));
        assert_eq!(field, "b");
    } else {
        panic!("expected member access");
    }
}

#[test]
fn parse_chained_member_access() {
    // a.b.c should parse as (a.b).c
    if let Expr::Member { expr, field } = common::assert_parses("a.b.c").node {
        assert_eq!(field, "c");
        if let Expr::Member {
            expr: inner_expr,
            field: inner_field,
        } = &expr.node
        {
            assert_eq!(inner_expr.node, Expr::Ident("a".to_string()));
            assert_eq!(inner_field, "b");
        } else {
            panic!("expected inner member access");
        }
    } else {
        panic!("expected outer member access");
    }
}

#[test]
fn parse_index_access() {
    if let Expr::Index { expr, index } = common::assert_parses("a[0]").node {
        assert_eq!(expr.node, Expr::Ident("a".to_string()));
        assert_eq!(index.node, Expr::Int(0));
    } else {
        panic!("expected index access");
    }
}

#[test]
fn parse_index_with_expression() {
    if let Expr::Index { expr, index } = common::assert_parses("a[i + 1]").node {
        assert_eq!(expr.node, Expr::Ident("a".to_string()));
        assert!(matches!(index.node, Expr::Binary { op: BinaryOp::Add, .. }));
    } else {
        panic!("expected index access");
    }
}

// ============================================================================
// Function call tests
// ============================================================================

#[test]
fn parse_function_call_no_args() {
    if let Expr::Call { expr, args } = common::assert_parses("f()").node {
        assert_eq!(expr.node, Expr::Ident("f".to_string()));
        assert!(args.is_empty());
    } else {
        panic!("expected call");
    }
}

#[test]
fn parse_function_call_with_args() {
    if let Expr::Call { expr, args } = common::assert_parses("f(x, y)").node {
        assert_eq!(expr.node, Expr::Ident("f".to_string()));
        assert_eq!(args.len(), 2);
        assert_eq!(args[0].node, Expr::Ident("x".to_string()));
        assert_eq!(args[1].node, Expr::Ident("y".to_string()));
    } else {
        panic!("expected call");
    }
}

#[test]
fn parse_method_call() {
    // a.method(x) should parse as (a.method)(x)
    if let Expr::Call { expr, args } = common::assert_parses("a.method(x)").node {
        if let Expr::Member {
            expr: member_expr,
            field,
        } = &expr.node
        {
            assert_eq!(member_expr.node, Expr::Ident("a".to_string()));
            assert_eq!(field, "method");
            assert_eq!(args.len(), 1);
        } else {
            panic!("expected member access for method");
        }
    } else {
        panic!("expected call");
    }
}

// ============================================================================
// Ternary conditional tests
// ============================================================================

#[test]
fn parse_ternary() {
    if let Expr::Ternary {
        cond,
        then_expr,
        else_expr,
    } = common::assert_parses("a ? b : c").node
    {
        assert_eq!(cond.node, Expr::Ident("a".to_string()));
        assert_eq!(then_expr.node, Expr::Ident("b".to_string()));
        assert_eq!(else_expr.node, Expr::Ident("c".to_string()));
    } else {
        panic!("expected ternary");
    }
}

#[test]
fn parse_nested_ternary() {
    // a ? b : c ? d : e should parse as a ? b : (c ? d : e)
    if let Expr::Ternary {
        cond,
        then_expr,
        else_expr,
    } = common::assert_parses("a ? b : c ? d : e").node
    {
        assert_eq!(cond.node, Expr::Ident("a".to_string()));
        assert_eq!(then_expr.node, Expr::Ident("b".to_string()));
        assert!(matches!(else_expr.node, Expr::Ternary { .. }));
    } else {
        panic!("expected ternary");
    }
}

// ============================================================================
// Complex expression tests
// ============================================================================

#[test]
fn parse_complex_expression() {
    // From CEL spec examples
    common::assert_parses("account.balance >= transaction.withdrawal");
    common::assert_parses("request.auth.claims.group == 'admin'");
    common::assert_parses("size(googletrans) > 100 && startsWith(googletrans, 'test')");
}

#[test]
fn parse_expression_with_parentheses() {
    // (1 + 2) * 3 should have different structure than 1 + 2 * 3
    if let Expr::Binary { op, left, .. } = common::assert_parses("(1 + 2) * 3").node {
        assert_eq!(op, BinaryOp::Mul);
        assert!(matches!(left.node, Expr::Binary { op: BinaryOp::Add, .. }));
    } else {
        panic!("expected multiplication at top level");
    }
}

// ============================================================================
// Span tests
// ============================================================================

#[test]
fn span_tracks_position() {
    let ast = common::assert_parses("123");
    assert_eq!(ast.span.start, 0);
    assert_eq!(ast.span.end, 3);
}

#[test]
fn span_binary_covers_operands() {
    let ast = common::assert_parses("1 + 2");
    assert_eq!(ast.span.start, 0);
    assert_eq!(ast.span.end, 5);
}

// ============================================================================
// Root identifier tests
// ============================================================================

#[test]
fn parse_root_identifier() {
    // .name should parse as RootIdent
    assert_eq!(
        common::assert_parses(".foo").node,
        Expr::RootIdent("foo".to_string())
    );
}

#[test]
fn parse_root_identifier_in_expression() {
    // .x + y should work
    if let Expr::Binary { op, left, right } = common::assert_parses(".x + y").node {
        assert_eq!(op, BinaryOp::Add);
        assert_eq!(left.node, Expr::RootIdent("x".to_string()));
        assert_eq!(right.node, Expr::Ident("y".to_string()));
    } else {
        panic!("expected binary");
    }
}

#[test]
fn parse_root_identifier_with_member_access() {
    // .x.y should parse as (.x).y
    if let Expr::Member { expr, field } = common::assert_parses(".x.y").node {
        assert_eq!(expr.node, Expr::RootIdent("x".to_string()));
        assert_eq!(field, "y");
    } else {
        panic!("expected member access");
    }
}

// ============================================================================
// Struct literal tests
// ============================================================================

#[test]
fn parse_struct_literal_simple() {
    // Foo{a: 1}
    if let Expr::Struct { type_name, fields } = common::assert_parses("Foo{a: 1}").node {
        assert_eq!(type_name.node, Expr::Ident("Foo".to_string()));
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].0, "a");
        assert_eq!(fields[0].1.node, Expr::Int(1));
    } else {
        panic!("expected struct literal");
    }
}

#[test]
fn parse_struct_literal_multiple_fields() {
    // Point{x: 1, y: 2}
    if let Expr::Struct { type_name, fields } = common::assert_parses("Point{x: 1, y: 2}").node {
        assert_eq!(type_name.node, Expr::Ident("Point".to_string()));
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].0, "x");
        assert_eq!(fields[1].0, "y");
    } else {
        panic!("expected struct literal");
    }
}

#[test]
fn parse_struct_literal_qualified_type() {
    // pkg.Type{field: value}
    if let Expr::Struct { type_name, fields } = common::assert_parses("pkg.Type{field: 42}").node {
        if let Expr::Member { expr, field } = &type_name.node {
            assert_eq!(expr.node, Expr::Ident("pkg".to_string()));
            assert_eq!(field, "Type");
        } else {
            panic!("expected member access for type name");
        }
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].0, "field");
    } else {
        panic!("expected struct literal");
    }
}

#[test]
fn parse_struct_literal_empty() {
    // Foo{}
    if let Expr::Struct { type_name, fields } = common::assert_parses("Foo{}").node {
        assert_eq!(type_name.node, Expr::Ident("Foo".to_string()));
        assert!(fields.is_empty());
    } else {
        panic!("expected struct literal");
    }
}

#[test]
fn parse_struct_literal_trailing_comma() {
    // Foo{a: 1,}
    if let Expr::Struct { type_name, fields } = common::assert_parses("Foo{a: 1,}").node {
        assert_eq!(type_name.node, Expr::Ident("Foo".to_string()));
        assert_eq!(fields.len(), 1);
    } else {
        panic!("expected struct literal");
    }
}
