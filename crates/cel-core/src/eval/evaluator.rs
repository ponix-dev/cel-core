//! Tree-walking evaluator for CEL expressions.
//!
//! The evaluator performs depth-first traversal of the AST, evaluating
//! each node and returning a `Value`. It supports:
//!
//! - Arithmetic, comparison, and logical operators
//! - Short-circuit evaluation for `&&`, `||`, and ternary
//! - Function calls via the function registry
//! - Comprehension evaluation for macros like `all`, `exists`, `map`, `filter`
//! - Error propagation (errors are values in CEL)
//! - Proto message construction and field access

use std::collections::HashMap;
use std::sync::Arc;

use prost_reflect::{
    DynamicMessage, FieldDescriptor, Kind, MapKey as ProtoMapKey, MessageDescriptor, ReflectMessage,
};

use super::{
    time::{self, TimestampComponent},
    wkt,
    Activation, EvalError, FunctionRegistry, HierarchicalActivation, MapKey,
    OptionalValue, TypeValue, Value, ValueMap,
};
use crate::checker::ReferenceInfo;
use crate::types::{BinaryOp, Expr, ProtoTypeRegistry, SpannedExpr, UnaryOp};

/// The CEL expression evaluator.
///
/// Evaluates a CEL AST against an activation (variable bindings) and
/// function registry. Optionally supports proto message construction
/// when configured with a proto type registry.
pub struct Evaluator<'a> {
    activation: &'a dyn Activation,
    functions: &'a FunctionRegistry,
    /// Reference map from type checking (for qualified name resolution).
    reference_map: Option<&'a HashMap<i64, ReferenceInfo>>,
    /// Proto type registry for message construction.
    proto_types: Option<&'a ProtoTypeRegistry>,
    /// Container namespace for type resolution (C++ namespace rules).
    container: String,
    /// Abbreviations for qualified name shortcuts.
    abbreviations: Option<&'a HashMap<String, String>>,
}

impl<'a> Evaluator<'a> {
    /// Create a new evaluator.
    pub fn new(activation: &'a dyn Activation, functions: &'a FunctionRegistry) -> Self {
        Self {
            activation,
            functions,
            reference_map: None,
            proto_types: None,
            container: String::new(),
            abbreviations: None,
        }
    }

    /// Set the reference map for qualified name resolution (builder pattern).
    pub fn with_reference_map(mut self, map: &'a HashMap<i64, ReferenceInfo>) -> Self {
        self.reference_map = Some(map);
        self
    }

    /// Set the proto type registry for message construction (builder pattern).
    pub fn with_proto_types(mut self, registry: &'a ProtoTypeRegistry) -> Self {
        self.proto_types = Some(registry);
        self
    }

    /// Set the container namespace for type resolution (builder pattern).
    ///
    /// The container is used for resolving unqualified type names following
    /// C++ namespace rules. For example, with container "cel.expr.conformance.proto3"
    /// and type name "TestAllTypes", resolution tries:
    /// 1. cel.expr.conformance.proto3.TestAllTypes
    /// 2. cel.expr.conformance.TestAllTypes
    /// 3. cel.expr.TestAllTypes
    /// 4. cel.TestAllTypes
    /// 5. TestAllTypes
    pub fn with_container(mut self, container: &str) -> Self {
        self.container = container.to_string();
        self
    }

    /// Set abbreviations for qualified name resolution (builder pattern).
    ///
    /// Abbreviations allow short names to be used instead of fully-qualified
    /// type names in expressions.
    pub fn with_abbreviations(mut self, abbreviations: &'a HashMap<String, String>) -> Self {
        self.abbreviations = Some(abbreviations);
        self
    }

    /// Evaluate an expression.
    pub fn eval(&self, expr: &SpannedExpr) -> Value {
        self.eval_expr(expr)
    }

    /// Create a child evaluator with a new activation but preserving other settings.
    fn child_evaluator<'b>(&'b self, activation: &'b dyn Activation) -> Evaluator<'b>
    where
        'a: 'b,
    {
        let mut eval = Evaluator::new(activation, self.functions);
        if let Some(ref_map) = self.reference_map {
            eval = eval.with_reference_map(ref_map);
        }
        if let Some(proto_types) = self.proto_types {
            eval = eval.with_proto_types(proto_types);
        }
        if !self.container.is_empty() {
            eval = eval.with_container(&self.container);
        }
        if let Some(abbreviations) = self.abbreviations {
            eval = eval.with_abbreviations(abbreviations);
        }
        eval
    }

    fn eval_expr(&self, expr: &SpannedExpr) -> Value {
        // Check reference_map for pre-resolved constant values (enum values, etc.)
        if let Some(ref_map) = self.reference_map {
            if let Some(ref_info) = ref_map.get(&expr.id) {
                if let Some(ref value) = ref_info.value {
                    return Value::from(value.clone());
                }
            }
        }
        match &expr.node {
            // Literals
            Expr::Null => Value::Null,
            Expr::Bool(b) => Value::Bool(*b),
            Expr::Int(i) => Value::Int(*i),
            Expr::UInt(u) => Value::UInt(*u),
            Expr::Float(f) => Value::Double(*f),
            Expr::String(s) => Value::String(Arc::from(s.as_str())),
            Expr::Bytes(b) => Value::Bytes(Arc::from(b.as_slice())),

            // Identifiers
            Expr::Ident(name) | Expr::RootIdent(name) => self.eval_ident(name),

            // Collections
            Expr::List(elements) => self.eval_list(elements),
            Expr::Map(entries) => self.eval_map(entries),

            // Operations
            Expr::Unary { op, expr } => self.eval_unary(*op, expr),
            Expr::Binary { op, left, right } => self.eval_binary(*op, left, right),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => self.eval_ternary(cond, then_expr, else_expr),

            // Access
            Expr::Member {
                expr,
                field,
                optional,
            } => self.eval_member(expr, field, *optional),
            Expr::Index {
                expr,
                index,
                optional,
            } => self.eval_index(expr, index, *optional),
            Expr::Call { expr, args } => self.eval_call(expr, args),
            Expr::Struct { type_name, fields } => self.eval_struct(type_name, fields),

            // Comprehension
            Expr::Comprehension {
                iter_var,
                iter_var2,
                iter_range,
                accu_var,
                accu_init,
                loop_condition,
                loop_step,
                result,
            } => self.eval_comprehension(
                iter_var,
                iter_var2,
                iter_range,
                accu_var,
                accu_init,
                loop_condition,
                loop_step,
                result,
            ),

            // Member test
            Expr::MemberTestOnly { expr, field } => self.eval_member_test(expr, field),

            // Variable binding
            Expr::Bind {
                var_name,
                init,
                body,
            } => self.eval_bind(var_name, init, body),

            // Error placeholder
            Expr::Error => Value::error(EvalError::internal("evaluated error expression")),
        }
    }

    fn eval_ident(&self, name: &str) -> Value {
        // Check for type constants
        match name {
            "null_type" => return Value::Type(TypeValue::null_type()),
            "bool" => return Value::Type(TypeValue::bool_type()),
            "int" => return Value::Type(TypeValue::int_type()),
            "uint" => return Value::Type(TypeValue::uint_type()),
            "double" => return Value::Type(TypeValue::double_type()),
            "string" => return Value::Type(TypeValue::string_type()),
            "bytes" => return Value::Type(TypeValue::bytes_type()),
            "list" => return Value::Type(TypeValue::list_type()),
            "map" => return Value::Type(TypeValue::map_type()),
            "type" => return Value::Type(TypeValue::type_type()),
            _ => {}
        }

        // Look up in activation
        self.activation
            .resolve(name)
            .unwrap_or_else(|| Value::error(EvalError::unknown_identifier(name)))
    }

    fn eval_list(&self, elements: &[crate::types::ListElement]) -> Value {
        let mut values = Vec::with_capacity(elements.len());

        for elem in elements {
            let value = self.eval_expr(&elem.expr);

            // Propagate errors
            if value.is_error() {
                return value;
            }

            if elem.optional {
                // Optional list element: only add if present
                match value {
                    Value::Optional(OptionalValue::Some(v)) => values.push(*v),
                    Value::Optional(OptionalValue::None) => {} // Skip absent optionals
                    _ => values.push(value),
                }
            } else {
                values.push(value);
            }
        }

        Value::List(Arc::from(values))
    }

    fn eval_map(&self, entries: &[crate::types::MapEntry]) -> Value {
        let mut map = ValueMap::new();

        for entry in entries {
            let key = self.eval_expr(&entry.key);
            if key.is_error() {
                return key;
            }

            let value = self.eval_expr(&entry.value);
            if value.is_error() {
                return value;
            }

            // Handle optional entries
            if entry.optional {
                match value {
                    Value::Optional(OptionalValue::Some(v)) => {
                        if let Some(map_key) = MapKey::from_value(&key) {
                            map.insert(map_key, *v);
                        } else {
                            return Value::error(EvalError::type_mismatch(
                                "valid map key",
                                &key.cel_type().display_name(),
                            ));
                        }
                    }
                    Value::Optional(OptionalValue::None) => {} // Skip absent optionals
                    _ => {
                        if let Some(map_key) = MapKey::from_value(&key) {
                            map.insert(map_key, value);
                        } else {
                            return Value::error(EvalError::type_mismatch(
                                "valid map key",
                                &key.cel_type().display_name(),
                            ));
                        }
                    }
                }
            } else {
                if let Some(map_key) = MapKey::from_value(&key) {
                    map.insert(map_key, value);
                } else {
                    return Value::error(EvalError::type_mismatch(
                        "valid map key",
                        &key.cel_type().display_name(),
                    ));
                }
            }
        }

        Value::Map(Arc::new(map))
    }

    fn eval_unary(&self, op: UnaryOp, expr: &SpannedExpr) -> Value {
        let value = self.eval_expr(expr);
        if value.is_error() {
            return value;
        }

        match op {
            UnaryOp::Neg => self.eval_negate(value),
            UnaryOp::Not => self.eval_not(value),
        }
    }

    fn eval_negate(&self, value: Value) -> Value {
        match value {
            Value::Int(i) => i
                .checked_neg()
                .map(Value::Int)
                .unwrap_or_else(|| Value::error(EvalError::overflow("integer negation overflow"))),
            Value::Double(d) => Value::Double(-d),
            _ => Value::error(EvalError::type_mismatch(
                "int or double",
                &value.cel_type().display_name(),
            )),
        }
    }

    fn eval_not(&self, value: Value) -> Value {
        match value {
            Value::Bool(b) => Value::Bool(!b),
            _ => Value::error(EvalError::type_mismatch(
                "bool",
                &value.cel_type().display_name(),
            )),
        }
    }

    fn eval_binary(&self, op: BinaryOp, left: &SpannedExpr, right: &SpannedExpr) -> Value {
        // Short-circuit evaluation for && and ||
        match op {
            BinaryOp::And => return self.eval_and(left, right),
            BinaryOp::Or => return self.eval_or(left, right),
            _ => {}
        }

        // Evaluate both operands
        let left_val = self.eval_expr(left);
        if left_val.is_error() {
            return left_val;
        }

        let right_val = self.eval_expr(right);
        if right_val.is_error() {
            return right_val;
        }

        match op {
            BinaryOp::Add => self.eval_add(left_val, right_val),
            BinaryOp::Sub => self.eval_sub(left_val, right_val),
            BinaryOp::Mul => self.eval_mul(left_val, right_val),
            BinaryOp::Div => self.eval_div(left_val, right_val),
            BinaryOp::Mod => self.eval_mod(left_val, right_val),
            BinaryOp::Eq => self.eval_eq(left_val, right_val),
            BinaryOp::Ne => self.eval_ne(left_val, right_val),
            BinaryOp::Lt => self.eval_lt(left_val, right_val),
            BinaryOp::Le => self.eval_le(left_val, right_val),
            BinaryOp::Gt => self.eval_gt(left_val, right_val),
            BinaryOp::Ge => self.eval_ge(left_val, right_val),
            BinaryOp::In => self.eval_in(left_val, right_val),
            BinaryOp::And | BinaryOp::Or => unreachable!("handled above"),
        }
    }

    fn eval_and(&self, left: &SpannedExpr, right: &SpannedExpr) -> Value {
        let left_val = self.eval_expr(left);

        // Short-circuit: if left is false, return false
        match &left_val {
            Value::Bool(false) => return Value::Bool(false),
            Value::Bool(true) => {}
            Value::Error(_) => {
                // CEL semantics: evaluate right side, return error if both error or if right is true
                let right_val = self.eval_expr(right);
                return match right_val {
                    Value::Bool(false) => Value::Bool(false),
                    _ => left_val, // Return the error
                };
            }
            // Non-boolean type: apply commutative logic same as errors
            _ => {
                let right_val = self.eval_expr(right);
                return match right_val {
                    Value::Bool(false) => Value::Bool(false),
                    _ => Value::error(EvalError::no_matching_overload("_&&_")),
                };
            }
        }

        // Left is true, evaluate right
        let right_val = self.eval_expr(right);
        match &right_val {
            Value::Bool(_) | Value::Error(_) => right_val,
            _ => Value::error(EvalError::no_matching_overload("_&&_")),
        }
    }

    fn eval_or(&self, left: &SpannedExpr, right: &SpannedExpr) -> Value {
        let left_val = self.eval_expr(left);

        // Short-circuit: if left is true, return true
        match &left_val {
            Value::Bool(true) => return Value::Bool(true),
            Value::Bool(false) => {}
            Value::Error(_) => {
                // CEL semantics: evaluate right side, return error if both error or if right is false
                let right_val = self.eval_expr(right);
                return match right_val {
                    Value::Bool(true) => Value::Bool(true),
                    _ => left_val, // Return the error
                };
            }
            // Non-boolean type: apply commutative logic same as errors
            _ => {
                let right_val = self.eval_expr(right);
                return match right_val {
                    Value::Bool(true) => Value::Bool(true),
                    _ => Value::error(EvalError::no_matching_overload("_||_")),
                };
            }
        }

        // Left is false, evaluate right
        let right_val = self.eval_expr(right);
        match &right_val {
            Value::Bool(_) | Value::Error(_) => right_val,
            _ => Value::error(EvalError::no_matching_overload("_||_")),
        }
    }

    fn eval_add(&self, left: Value, right: Value) -> Value {
        match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => a
                .checked_add(*b)
                .map(Value::Int)
                .unwrap_or_else(|| Value::error(EvalError::overflow("integer addition overflow"))),
            (Value::UInt(a), Value::UInt(b)) => a
                .checked_add(*b)
                .map(Value::UInt)
                .unwrap_or_else(|| Value::error(EvalError::overflow("unsigned addition overflow"))),
            (Value::Double(a), Value::Double(b)) => Value::Double(a + b),
            (Value::String(a), Value::String(b)) => {
                let mut result = String::with_capacity(a.len() + b.len());
                result.push_str(a);
                result.push_str(b);
                Value::String(Arc::from(result))
            }
            (Value::Bytes(a), Value::Bytes(b)) => {
                let mut result = Vec::with_capacity(a.len() + b.len());
                result.extend_from_slice(a);
                result.extend_from_slice(b);
                Value::Bytes(Arc::from(result))
            }
            (Value::List(a), Value::List(b)) => {
                let mut result = Vec::with_capacity(a.len() + b.len());
                result.extend(a.iter().cloned());
                result.extend(b.iter().cloned());
                Value::List(Arc::from(result))
            }
            (Value::Timestamp(t), Value::Duration(d)) => {
                // Normalize: timestamp nanos are always 0..999_999_999
                // Duration nanos can be negative for negative durations
                let nanos = t.nanos as i64 + d.nanos as i64;
                let (extra_secs, nanos) = normalize_nanos(nanos);

                match t
                    .seconds
                    .checked_add(d.seconds)
                    .and_then(|s| s.checked_add(extra_secs))
                {
                    Some(seconds) => {
                        let ts = super::Timestamp::new(seconds, nanos);
                        if ts.is_valid() {
                            Value::Timestamp(ts)
                        } else {
                            Value::error(EvalError::range_error(
                                "timestamp out of range: must be between year 0001 and 9999",
                            ))
                        }
                    }
                    None => Value::error(EvalError::overflow("timestamp addition overflow")),
                }
            }
            (Value::Duration(d), Value::Timestamp(t)) => {
                let nanos = t.nanos as i64 + d.nanos as i64;
                let (extra_secs, nanos) = normalize_nanos(nanos);

                match t
                    .seconds
                    .checked_add(d.seconds)
                    .and_then(|s| s.checked_add(extra_secs))
                {
                    Some(seconds) => {
                        let ts = super::Timestamp::new(seconds, nanos);
                        if ts.is_valid() {
                            Value::Timestamp(ts)
                        } else {
                            Value::error(EvalError::range_error(
                                "timestamp out of range: must be between year 0001 and 9999",
                            ))
                        }
                    }
                    None => Value::error(EvalError::overflow("timestamp addition overflow")),
                }
            }
            (Value::Duration(a), Value::Duration(b)) => {
                let nanos = a.nanos as i64 + b.nanos as i64;
                let (extra_secs, nanos) = normalize_nanos(nanos);

                match a
                    .seconds
                    .checked_add(b.seconds)
                    .and_then(|s| s.checked_add(extra_secs))
                {
                    Some(seconds) => {
                        let d = super::Duration::new(seconds, nanos);
                        if d.is_valid() {
                            Value::Duration(d)
                        } else {
                            Value::error(EvalError::range_error(
                                "duration out of range: must be within approximately 10000 years",
                            ))
                        }
                    }
                    None => Value::error(EvalError::overflow("duration addition overflow")),
                }
            }
            _ => Value::error(EvalError::no_matching_overload("_+_")),
        }
    }

    fn eval_sub(&self, left: Value, right: Value) -> Value {
        match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => a.checked_sub(*b).map(Value::Int).unwrap_or_else(|| {
                Value::error(EvalError::overflow("integer subtraction overflow"))
            }),
            (Value::UInt(a), Value::UInt(b)) => a.checked_sub(*b).map(Value::UInt).unwrap_or_else(
                || Value::error(EvalError::overflow("unsigned subtraction overflow")),
            ),
            (Value::Double(a), Value::Double(b)) => Value::Double(a - b),
            (Value::Timestamp(a), Value::Timestamp(b)) => {
                let nanos = a.nanos as i64 - b.nanos as i64;
                let (extra_secs, nanos) = normalize_nanos(nanos);

                match a
                    .seconds
                    .checked_sub(b.seconds)
                    .and_then(|s| s.checked_add(extra_secs))
                {
                    Some(seconds) => {
                        let d = super::Duration::new(seconds, nanos);
                        if d.is_valid() {
                            Value::Duration(d)
                        } else {
                            Value::error(EvalError::range_error(
                                "duration out of range: must be within approximately 10000 years",
                            ))
                        }
                    }
                    None => Value::error(EvalError::overflow("timestamp subtraction overflow")),
                }
            }
            (Value::Timestamp(t), Value::Duration(d)) => {
                let nanos = t.nanos as i64 - d.nanos as i64;
                let (extra_secs, nanos) = normalize_nanos(nanos);

                match t
                    .seconds
                    .checked_sub(d.seconds)
                    .and_then(|s| s.checked_add(extra_secs))
                {
                    Some(seconds) => {
                        let ts = super::Timestamp::new(seconds, nanos);
                        if ts.is_valid() {
                            Value::Timestamp(ts)
                        } else {
                            Value::error(EvalError::range_error(
                                "timestamp out of range: must be between year 0001 and 9999",
                            ))
                        }
                    }
                    None => Value::error(EvalError::overflow("timestamp subtraction overflow")),
                }
            }
            (Value::Duration(a), Value::Duration(b)) => {
                let nanos = a.nanos as i64 - b.nanos as i64;
                let (extra_secs, nanos) = normalize_nanos(nanos);

                match a
                    .seconds
                    .checked_sub(b.seconds)
                    .and_then(|s| s.checked_add(extra_secs))
                {
                    Some(seconds) => {
                        let d = super::Duration::new(seconds, nanos);
                        if d.is_valid() {
                            Value::Duration(d)
                        } else {
                            Value::error(EvalError::range_error(
                                "duration out of range: must be within approximately 10000 years",
                            ))
                        }
                    }
                    None => Value::error(EvalError::overflow("duration subtraction overflow")),
                }
            }
            _ => Value::error(EvalError::no_matching_overload("_-_")),
        }
    }

    fn eval_mul(&self, left: Value, right: Value) -> Value {
        match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => a.checked_mul(*b).map(Value::Int).unwrap_or_else(|| {
                Value::error(EvalError::overflow("integer multiplication overflow"))
            }),
            (Value::UInt(a), Value::UInt(b)) => a.checked_mul(*b).map(Value::UInt).unwrap_or_else(
                || Value::error(EvalError::overflow("unsigned multiplication overflow")),
            ),
            (Value::Double(a), Value::Double(b)) => Value::Double(a * b),
            _ => Value::error(EvalError::no_matching_overload("_*_")),
        }
    }

    fn eval_div(&self, left: Value, right: Value) -> Value {
        match (&left, &right) {
            (Value::Int(_), Value::Int(0)) => Value::error(EvalError::division_by_zero()),
            (Value::Int(a), Value::Int(b)) => a.checked_div(*b).map(Value::Int).unwrap_or_else(|| {
                Value::error(EvalError::overflow("integer division overflow"))
            }),
            (Value::UInt(_), Value::UInt(0)) => Value::error(EvalError::division_by_zero()),
            (Value::UInt(a), Value::UInt(b)) => Value::UInt(a / b),
            (Value::Double(a), Value::Double(b)) => Value::Double(a / b),
            _ => Value::error(EvalError::no_matching_overload("_/_")),
        }
    }

    fn eval_mod(&self, left: Value, right: Value) -> Value {
        match (&left, &right) {
            (Value::Int(_), Value::Int(0)) => Value::error(EvalError::modulo_by_zero()),
            (Value::Int(a), Value::Int(b)) => a.checked_rem(*b).map(Value::Int).unwrap_or_else(|| {
                Value::error(EvalError::overflow("integer modulo overflow"))
            }),
            (Value::UInt(_), Value::UInt(0)) => Value::error(EvalError::modulo_by_zero()),
            (Value::UInt(a), Value::UInt(b)) => Value::UInt(a % b),
            _ => Value::error(EvalError::no_matching_overload("_%_")),
        }
    }

    fn eval_eq(&self, left: Value, right: Value) -> Value {
        Value::Bool(left == right)
    }

    fn eval_ne(&self, left: Value, right: Value) -> Value {
        Value::Bool(left != right)
    }

    fn eval_lt(&self, left: Value, right: Value) -> Value {
        match left.compare(&right) {
            Some(std::cmp::Ordering::Less) => Value::Bool(true),
            Some(_) => Value::Bool(false),
            None => Value::error(EvalError::no_matching_overload("_<_")),
        }
    }

    fn eval_le(&self, left: Value, right: Value) -> Value {
        match left.compare(&right) {
            Some(std::cmp::Ordering::Less) | Some(std::cmp::Ordering::Equal) => Value::Bool(true),
            Some(_) => Value::Bool(false),
            None => Value::error(EvalError::no_matching_overload("_<=_")),
        }
    }

    fn eval_gt(&self, left: Value, right: Value) -> Value {
        match left.compare(&right) {
            Some(std::cmp::Ordering::Greater) => Value::Bool(true),
            Some(_) => Value::Bool(false),
            None => Value::error(EvalError::no_matching_overload("_>_")),
        }
    }

    fn eval_ge(&self, left: Value, right: Value) -> Value {
        match left.compare(&right) {
            Some(std::cmp::Ordering::Greater) | Some(std::cmp::Ordering::Equal) => Value::Bool(true),
            Some(_) => Value::Bool(false),
            None => Value::error(EvalError::no_matching_overload("_>=_")),
        }
    }

    fn eval_in(&self, left: Value, right: Value) -> Value {
        match &right {
            Value::List(list) => {
                for elem in list.iter() {
                    if left == *elem {
                        return Value::Bool(true);
                    }
                }
                Value::Bool(false)
            }
            Value::Map(map) => {
                // Handle double keys: coerce exact integers to int/uint for lookup
                if let Value::Double(d) = &left {
                    if d.is_finite() && d.fract() == 0.0 {
                        let as_int = *d as i64;
                        let int_key = MapKey::Int(as_int);
                        return Value::Bool(map.contains_key_with_numeric_coercion(&int_key));
                    } else {
                        return Value::Bool(false);
                    }
                }
                if let Some(key) = MapKey::from_value(&left) {
                    Value::Bool(map.contains_key_with_numeric_coercion(&key))
                } else {
                    Value::error(EvalError::type_mismatch(
                        "valid map key",
                        &left.cel_type().display_name(),
                    ))
                }
            }
            _ => Value::error(EvalError::no_matching_overload("_in_")),
        }
    }

    fn eval_ternary(
        &self,
        cond: &SpannedExpr,
        then_expr: &SpannedExpr,
        else_expr: &SpannedExpr,
    ) -> Value {
        let cond_val = self.eval_expr(cond);

        match cond_val {
            Value::Bool(true) => self.eval_expr(then_expr),
            Value::Bool(false) => self.eval_expr(else_expr),
            Value::Error(_) => cond_val,
            _ => Value::error(EvalError::type_mismatch(
                "bool",
                &cond_val.cel_type().display_name(),
            )),
        }
    }

    fn eval_member(&self, expr: &SpannedExpr, field: &str, optional: bool) -> Value {
        let value = self.eval_expr(expr);

        if value.is_error() {
            return value;
        }

        // Handle optional select
        if optional {
            match &value {
                Value::Optional(OptionalValue::None) => return Value::Optional(OptionalValue::None),
                Value::Optional(OptionalValue::Some(inner)) => {
                    return self.access_field(inner, field, true);
                }
                _ => {}
            }
        }

        self.access_field(&value, field, optional)
    }

    fn access_field(&self, value: &Value, field: &str, optional: bool) -> Value {
        match value {
            Value::Map(map) => {
                let key = MapKey::String(Arc::from(field));
                match map.get(&key) {
                    Some(v) => {
                        if optional {
                            Value::optional_some(v.clone())
                        } else {
                            v.clone()
                        }
                    }
                    None => {
                        if optional {
                            Value::optional_none()
                        } else {
                            Value::error(EvalError::key_not_found(field))
                        }
                    }
                }
            }
            Value::Proto(proto) => {
                let descriptor = proto.descriptor();
                match descriptor.get_field_by_name(field) {
                    Some(field_desc) => {
                        // For wrapper/well-known message fields that support presence,
                        // return null if the field is not set.
                        // This applies to wrapper types (e.g., Int64Value, StringValue)
                        // and other WKT message fields (Any, Value, etc.)
                        if field_desc.supports_presence()
                            && !proto.message().has_field(&field_desc)
                        {
                            if let Kind::Message(msg_desc) = field_desc.kind() {
                                let msg_name = msg_desc.full_name();
                                // Unset ListValue → empty list, unset Struct → empty map
                                if msg_name == "google.protobuf.ListValue" {
                                    let result = Value::List(Arc::from(Vec::<Value>::new()));
                                    return if optional {
                                        Value::optional_some(result)
                                    } else {
                                        result
                                    };
                                }
                                if msg_name == "google.protobuf.Struct" {
                                    let result = Value::Map(Arc::new(ValueMap::new()));
                                    return if optional {
                                        Value::optional_some(result)
                                    } else {
                                        result
                                    };
                                }
                                let is_wkt = msg_name.starts_with("google.protobuf.");
                                if is_wkt {
                                    if optional {
                                        return Value::optional_some(Value::Null);
                                    } else {
                                        return Value::Null;
                                    }
                                }
                            }
                        }
                        let proto_value = proto.message().get_field(&field_desc);
                        let cel_value = self.proto_reflect_to_value(proto_value, &field_desc);
                        if optional {
                            Value::optional_some(cel_value)
                        } else {
                            cel_value
                        }
                    }
                    None => {
                        // Try extension field lookup
                        if let Some(registry) = self.proto_types {
                            if let Some(ext) = registry.get_extension_by_name(field) {
                                if ext.containing_message() == descriptor {
                                    let proto_value = proto.message().get_extension(&ext);
                                    let cel_value =
                                        self.extension_value_to_cel(proto_value, &ext);
                                    return if optional {
                                        Value::optional_some(cel_value)
                                    } else {
                                        cel_value
                                    };
                                }
                            }
                        }
                        if optional {
                            Value::optional_none()
                        } else {
                            Value::error(EvalError::field_not_found(field))
                        }
                    }
                }
            }
            _ => {
                if optional {
                    Value::optional_none()
                } else {
                    Value::error(EvalError::field_not_found(field))
                }
            }
        }
    }

    /// Convert a prost_reflect Value to a CEL Value.
    fn proto_reflect_to_value(
        &self,
        proto_value: std::borrow::Cow<prost_reflect::Value>,
        field: &FieldDescriptor,
    ) -> Value {
        match proto_value.as_ref() {
            prost_reflect::Value::Bool(b) => Value::Bool(*b),
            prost_reflect::Value::I32(i) => Value::Int(*i as i64),
            prost_reflect::Value::I64(i) => Value::Int(*i),
            prost_reflect::Value::U32(u) => Value::UInt(*u as u64),
            prost_reflect::Value::U64(u) => Value::UInt(*u),
            prost_reflect::Value::F32(f) => Value::Double(*f as f64),
            prost_reflect::Value::F64(f) => Value::Double(*f),
            prost_reflect::Value::String(s) => Value::String(Arc::from(s.as_str())),
            prost_reflect::Value::Bytes(b) => Value::Bytes(Arc::from(b.as_ref())),
            prost_reflect::Value::EnumNumber(n) => Value::Int(*n as i64),
            prost_reflect::Value::Message(msg) => {
                // Unwrap well-known types
                self.maybe_unwrap_well_known(msg.clone())
            }
            prost_reflect::Value::List(list) => {
                let elem_kind = field.kind();
                let values: Vec<Value> = list
                    .iter()
                    .map(|v| self.proto_scalar_to_value(v, &elem_kind))
                    .collect();
                Value::List(Arc::from(values))
            }
            prost_reflect::Value::Map(map) => {
                let mut value_map = ValueMap::new();
                if let Kind::Message(map_entry) = field.kind() {
                    let value_field = map_entry.get_field_by_name("value");
                    for (k, v) in map {
                        let key = self.proto_map_key_to_value(k);
                        if let Some(map_key) = MapKey::from_value(&key) {
                            let value = if let Some(ref vf) = value_field {
                                self.proto_scalar_to_value(v, &vf.kind())
                            } else {
                                self.proto_scalar_to_value(v, &Kind::Double)
                            };
                            value_map.insert(map_key, value);
                        }
                    }
                }
                Value::Map(Arc::new(value_map))
            }
        }
    }

    /// Convert a scalar prost_reflect Value to a CEL Value.
    fn proto_scalar_to_value(&self, value: &prost_reflect::Value, kind: &Kind) -> Value {
        match value {
            prost_reflect::Value::Bool(b) => Value::Bool(*b),
            prost_reflect::Value::I32(i) => Value::Int(*i as i64),
            prost_reflect::Value::I64(i) => Value::Int(*i),
            prost_reflect::Value::U32(u) => Value::UInt(*u as u64),
            prost_reflect::Value::U64(u) => Value::UInt(*u),
            prost_reflect::Value::F32(f) => Value::Double(*f as f64),
            prost_reflect::Value::F64(f) => Value::Double(*f),
            prost_reflect::Value::String(s) => Value::String(Arc::from(s.as_str())),
            prost_reflect::Value::Bytes(b) => Value::Bytes(Arc::from(b.as_ref())),
            prost_reflect::Value::EnumNumber(n) => Value::Int(*n as i64),
            prost_reflect::Value::Message(msg) => self.maybe_unwrap_well_known(msg.clone()),
            prost_reflect::Value::List(list) => {
                let values: Vec<Value> = list
                    .iter()
                    .map(|v| self.proto_scalar_to_value(v, kind))
                    .collect();
                Value::List(Arc::from(values))
            }
            prost_reflect::Value::Map(_) => {
                // Nested maps not typically encountered here
                Value::error(EvalError::internal("nested maps not supported"))
            }
        }
    }

    /// Convert a prost_reflect MapKey to a CEL Value.
    fn proto_map_key_to_value(&self, key: &ProtoMapKey) -> Value {
        match key {
            ProtoMapKey::Bool(b) => Value::Bool(*b),
            ProtoMapKey::I32(i) => Value::Int(*i as i64),
            ProtoMapKey::I64(i) => Value::Int(*i),
            ProtoMapKey::U32(u) => Value::UInt(*u as u64),
            ProtoMapKey::U64(u) => Value::UInt(*u),
            ProtoMapKey::String(s) => Value::String(Arc::from(s.as_str())),
        }
    }

    /// Convert an extension field value to a CEL Value.
    fn extension_value_to_cel(
        &self,
        proto_value: std::borrow::Cow<prost_reflect::Value>,
        ext: &prost_reflect::ExtensionDescriptor,
    ) -> Value {
        match proto_value.as_ref() {
            prost_reflect::Value::Bool(b) => Value::Bool(*b),
            prost_reflect::Value::I32(i) => Value::Int(*i as i64),
            prost_reflect::Value::I64(i) => Value::Int(*i),
            prost_reflect::Value::U32(u) => Value::UInt(*u as u64),
            prost_reflect::Value::U64(u) => Value::UInt(*u),
            prost_reflect::Value::F32(f) => Value::Double(*f as f64),
            prost_reflect::Value::F64(f) => Value::Double(*f),
            prost_reflect::Value::String(s) => Value::String(Arc::from(s.as_str())),
            prost_reflect::Value::Bytes(b) => Value::Bytes(Arc::from(b.as_ref())),
            prost_reflect::Value::EnumNumber(n) => Value::Int(*n as i64),
            prost_reflect::Value::Message(msg) => self.maybe_unwrap_well_known(msg.clone()),
            prost_reflect::Value::List(list) => {
                let kind = ext.kind();
                let values: Vec<Value> = list
                    .iter()
                    .map(|v| self.proto_scalar_to_value(v, &kind))
                    .collect();
                Value::List(Arc::from(values))
            }
            prost_reflect::Value::Map(_) => {
                Value::error(EvalError::internal("extension maps not supported"))
            }
        }
    }

    fn eval_index(&self, expr: &SpannedExpr, index: &SpannedExpr, optional: bool) -> Value {
        let value = self.eval_expr(expr);
        if value.is_error() {
            return value;
        }

        let index_val = self.eval_expr(index);
        if index_val.is_error() {
            return index_val;
        }

        // Handle optional index
        if optional {
            match &value {
                Value::Optional(OptionalValue::None) => return Value::Optional(OptionalValue::None),
                Value::Optional(OptionalValue::Some(inner)) => {
                    return self.access_index(inner, &index_val, true);
                }
                _ => {}
            }
        }

        self.access_index(&value, &index_val, optional)
    }

    fn access_index(&self, value: &Value, index: &Value, optional: bool) -> Value {
        match value {
            Value::List(list) => {
                let idx = match index {
                    Value::Int(i) => *i,
                    Value::UInt(u) => *u as i64,
                    Value::Double(d) if d.is_finite() && d.fract() == 0.0 => *d as i64,
                    _ => {
                        return Value::error(EvalError::type_mismatch(
                            "int",
                            &index.cel_type().display_name(),
                        ))
                    }
                };

                // Handle negative indices
                let len = list.len() as i64;
                let actual_idx = if idx < 0 {
                    idx + len
                } else {
                    idx
                };

                if actual_idx < 0 || actual_idx >= len {
                    if optional {
                        Value::optional_none()
                    } else {
                        Value::error(EvalError::index_out_of_bounds(idx, list.len()))
                    }
                } else {
                    let result = list[actual_idx as usize].clone();
                    if optional {
                        Value::optional_some(result)
                    } else {
                        result
                    }
                }
            }
            Value::Map(map) => {
                // Handle double keys: coerce exact integers to int/uint for lookup
                let map_key = if let Value::Double(d) = index {
                    if d.is_finite() && d.fract() == 0.0 {
                        // Try as int key first, then uint
                        let as_int = *d as i64;
                        let int_key = MapKey::Int(as_int);
                        if let Some(v) = map.get_with_numeric_coercion(&int_key) {
                            let result = v.clone();
                            return if optional {
                                Value::optional_some(result)
                            } else {
                                result
                            };
                        }
                        // Key not found
                        return if optional {
                            Value::optional_none()
                        } else {
                            Value::error(EvalError::key_not_found(&format!("{}", index)))
                        };
                    } else {
                        return Value::error(EvalError::key_not_found(&format!("{}", index)));
                    }
                } else {
                    MapKey::from_value(index)
                };
                if let Some(key) = map_key {
                    match map.get_with_numeric_coercion(&key) {
                        Some(v) => {
                            if optional {
                                Value::optional_some(v.clone())
                            } else {
                                v.clone()
                            }
                        }
                        None => {
                            if optional {
                                Value::optional_none()
                            } else {
                                Value::error(EvalError::key_not_found(&format!("{}", index)))
                            }
                        }
                    }
                } else {
                    Value::error(EvalError::type_mismatch(
                        "valid map key",
                        &index.cel_type().display_name(),
                    ))
                }
            }
            Value::String(s) => {
                let idx = match index {
                    Value::Int(i) => *i,
                    _ => {
                        return Value::error(EvalError::type_mismatch(
                            "int",
                            &index.cel_type().display_name(),
                        ))
                    }
                };

                // Convert string to code points for proper indexing
                let chars: Vec<char> = s.chars().collect();
                let len = chars.len() as i64;
                let actual_idx = if idx < 0 { idx + len } else { idx };

                if actual_idx < 0 || actual_idx >= len {
                    if optional {
                        Value::optional_none()
                    } else {
                        Value::error(EvalError::index_out_of_bounds(idx, chars.len()))
                    }
                } else {
                    let result = Value::String(Arc::from(chars[actual_idx as usize].to_string()));
                    if optional {
                        Value::optional_some(result)
                    } else {
                        result
                    }
                }
            }
            Value::Bytes(bytes) => {
                let idx = match index {
                    Value::Int(i) => *i,
                    _ => {
                        return Value::error(EvalError::type_mismatch(
                            "int",
                            &index.cel_type().display_name(),
                        ))
                    }
                };

                let len = bytes.len() as i64;
                let actual_idx = if idx < 0 { idx + len } else { idx };

                if actual_idx < 0 || actual_idx >= len {
                    if optional {
                        Value::optional_none()
                    } else {
                        Value::error(EvalError::index_out_of_bounds(idx, bytes.len()))
                    }
                } else {
                    let result = Value::UInt(bytes[actual_idx as usize] as u64);
                    if optional {
                        Value::optional_some(result)
                    } else {
                        result
                    }
                }
            }
            _ => {
                if optional {
                    Value::optional_none()
                } else {
                    Value::error(EvalError::type_mismatch(
                        "list, map, string, or bytes",
                        &value.cel_type().display_name(),
                    ))
                }
            }
        }
    }

    fn eval_call(&self, expr: &SpannedExpr, args: &[SpannedExpr]) -> Value {
        // Determine function name and whether it's a member call
        let (func_name, receiver, is_member) = self.resolve_call_target(expr);

        // Evaluate arguments
        let mut arg_values = Vec::with_capacity(args.len() + if receiver.is_some() { 1 } else { 0 });

        if let Some(recv) = receiver {
            let recv_val = self.eval_expr(recv);
            if recv_val.is_error() {
                return recv_val;
            }
            arg_values.push(recv_val);
        }

        for arg in args {
            let val = self.eval_expr(arg);
            if val.is_error() {
                return val;
            }
            arg_values.push(val);
        }

        // Look up function in registry
        self.call_function(&func_name, &arg_values, is_member)
    }

    fn resolve_call_target<'b>(
        &self,
        expr: &'b SpannedExpr,
    ) -> (String, Option<&'b SpannedExpr>, bool) {
        match &expr.node {
            Expr::Ident(name) | Expr::RootIdent(name) => (name.clone(), None, false),
            Expr::Member {
                expr: recv,
                field,
                optional: _,
            } => (field.clone(), Some(recv.as_ref()), true),
            _ => ("".to_string(), None, false),
        }
    }

    fn call_function(&self, name: &str, args: &[Value], is_member: bool) -> Value {
        // Look up function
        let overloads = self.functions.find_overloads(name, args.len(), is_member);

        if overloads.is_empty() {
            // Check if it's a type conversion function
            if let Some(result) = self.try_type_conversion(name, args) {
                return result;
            }

            // Check for built-in functions handled specially
            if let Some(result) = self.try_builtin_function(name, args) {
                return result;
            }

            return Value::error(EvalError::unknown_function(name));
        }

        // Try each overload until one works
        // In a typed system, we'd use type information to select the right overload
        // For now, try them all and return the first non-error result
        for overload in &overloads {
            let result = overload.call(args);
            if !result.is_error() {
                return result;
            }
        }

        // All overloads failed, return the error from the first one
        overloads[0].call(args)
    }

    fn try_type_conversion(&self, name: &str, args: &[Value]) -> Option<Value> {
        if args.len() != 1 {
            return None;
        }

        let arg = &args[0];
        match name {
            "int" => Some(self.convert_to_int(arg)),
            "uint" => Some(self.convert_to_uint(arg)),
            "double" => Some(self.convert_to_double(arg)),
            "string" => Some(self.convert_to_string(arg)),
            "bytes" => Some(self.convert_to_bytes(arg)),
            "bool" => Some(self.convert_to_bool(arg)),
            "type" => Some(Value::Type(arg.type_value())),
            "dyn" => Some(arg.clone()),
            "timestamp" => Some(self.convert_to_timestamp(arg)),
            "duration" => Some(self.convert_to_duration(arg)),
            _ => None,
        }
    }

    fn convert_to_int(&self, value: &Value) -> Value {
        match value {
            Value::Int(i) => Value::Int(*i),
            Value::UInt(u) => {
                if *u > i64::MAX as u64 {
                    Value::error(EvalError::overflow("uint to int overflow"))
                } else {
                    Value::Int(*u as i64)
                }
            }
            Value::Double(d) => Value::Int(*d as i64),
            Value::String(s) => s
                .parse::<i64>()
                .map(Value::Int)
                .unwrap_or_else(|_| Value::error(EvalError::invalid_conversion("string", "int"))),
            Value::Timestamp(t) => Value::Int(t.seconds),
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "int",
            )),
        }
    }

    fn convert_to_uint(&self, value: &Value) -> Value {
        match value {
            Value::UInt(u) => Value::UInt(*u),
            Value::Int(i) => {
                if *i < 0 {
                    Value::error(EvalError::overflow("negative int to uint"))
                } else {
                    Value::UInt(*i as u64)
                }
            }
            Value::Double(d) => {
                if *d < 0.0 {
                    Value::error(EvalError::overflow("negative double to uint"))
                } else {
                    Value::UInt(*d as u64)
                }
            }
            Value::String(s) => s
                .parse::<u64>()
                .map(Value::UInt)
                .unwrap_or_else(|_| Value::error(EvalError::invalid_conversion("string", "uint"))),
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "uint",
            )),
        }
    }

    fn convert_to_double(&self, value: &Value) -> Value {
        match value {
            Value::Double(d) => Value::Double(*d),
            Value::Int(i) => Value::Double(*i as f64),
            Value::UInt(u) => Value::Double(*u as f64),
            Value::String(s) => s.parse::<f64>().map(Value::Double).unwrap_or_else(|_| {
                Value::error(EvalError::invalid_conversion("string", "double"))
            }),
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "double",
            )),
        }
    }

    fn convert_to_string(&self, value: &Value) -> Value {
        match value {
            Value::String(s) => Value::String(s.clone()),
            Value::Int(i) => Value::String(Arc::from(i.to_string())),
            Value::UInt(u) => Value::String(Arc::from(u.to_string())),
            Value::Double(d) => Value::String(Arc::from(format_double(*d))),
            Value::Bool(b) => Value::String(Arc::from(b.to_string())),
            Value::Bytes(b) => match std::str::from_utf8(b) {
                Ok(s) => Value::String(Arc::from(s)),
                Err(_) => Value::error(EvalError::invalid_conversion("bytes", "string")),
            },
            Value::Timestamp(t) => {
                // Format as RFC 3339 with nanoseconds
                Value::String(Arc::from(time::format_timestamp(t)))
            }
            Value::Duration(d) => {
                // Format as CEL duration string
                Value::String(Arc::from(time::format_duration(d)))
            }
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "string",
            )),
        }
    }

    fn convert_to_bytes(&self, value: &Value) -> Value {
        match value {
            Value::Bytes(b) => Value::Bytes(b.clone()),
            Value::String(s) => Value::Bytes(Arc::from(s.as_bytes())),
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "bytes",
            )),
        }
    }

    fn convert_to_bool(&self, value: &Value) -> Value {
        match value {
            Value::Bool(b) => Value::Bool(*b),
            Value::String(s) => match s.as_ref() {
                "true" => Value::Bool(true),
                "false" => Value::Bool(false),
                _ => Value::error(EvalError::invalid_conversion("string", "bool")),
            },
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "bool",
            )),
        }
    }

    fn convert_to_timestamp(&self, value: &Value) -> Value {
        match value {
            Value::Timestamp(t) => Value::Timestamp(*t),
            Value::String(s) => match time::parse_timestamp(s) {
                Ok(ts) => Value::Timestamp(ts),
                Err(e) => Value::error(EvalError::invalid_argument(e)),
            },
            Value::Int(i) => {
                let ts = super::Timestamp::from_seconds(*i);
                if ts.is_valid() {
                    Value::Timestamp(ts)
                } else {
                    Value::error(EvalError::range_error(
                        "timestamp out of range: must be between year 0001 and 9999",
                    ))
                }
            }
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "timestamp",
            )),
        }
    }

    fn convert_to_duration(&self, value: &Value) -> Value {
        match value {
            Value::Duration(d) => Value::Duration(*d),
            Value::String(s) => match time::parse_duration(s) {
                Ok(d) => Value::Duration(d),
                Err(e) => Value::error(EvalError::invalid_argument(e)),
            },
            Value::Int(i) => {
                let d = super::Duration::from_seconds(*i);
                if d.is_valid() {
                    Value::Duration(d)
                } else {
                    Value::error(EvalError::range_error(
                        "duration out of range: must be within approximately 10000 years",
                    ))
                }
            }
            _ => Value::error(EvalError::invalid_conversion(
                &value.cel_type().display_name(),
                "duration",
            )),
        }
    }

    fn try_builtin_function(&self, name: &str, args: &[Value]) -> Option<Value> {
        match name {
            "size" => {
                if args.len() != 1 {
                    return None;
                }
                Some(self.builtin_size(&args[0]))
            }
            "contains" => {
                if args.len() != 2 {
                    return None;
                }
                Some(self.builtin_contains(&args[0], &args[1]))
            }
            "startsWith" => {
                if args.len() != 2 {
                    return None;
                }
                Some(self.builtin_starts_with(&args[0], &args[1]))
            }
            "endsWith" => {
                if args.len() != 2 {
                    return None;
                }
                Some(self.builtin_ends_with(&args[0], &args[1]))
            }
            "matches" => {
                if args.len() != 2 {
                    return None;
                }
                Some(self.builtin_matches(&args[0], &args[1]))
            }
            // Timestamp accessors
            "getFullYear" => Some(self.timestamp_accessor(args, TimestampComponent::FullYear)),
            "getMonth" => Some(self.timestamp_accessor(args, TimestampComponent::Month)),
            "getDate" => Some(self.timestamp_accessor(args, TimestampComponent::Date)),
            "getDayOfMonth" => Some(self.timestamp_accessor(args, TimestampComponent::DayOfMonth)),
            "getDayOfWeek" => Some(self.timestamp_accessor(args, TimestampComponent::DayOfWeek)),
            "getDayOfYear" => Some(self.timestamp_accessor(args, TimestampComponent::DayOfYear)),
            "getHours" => Some(self.time_accessor_hours(args)),
            "getMinutes" => Some(self.time_accessor_minutes(args)),
            "getSeconds" => Some(self.time_accessor_seconds(args)),
            "getMilliseconds" => Some(self.time_accessor_milliseconds(args)),
            _ => None,
        }
    }

    /// Handle timestamp accessor functions that can take either:
    /// - 1 arg: timestamp (UTC)
    /// - 2 args: timestamp, timezone string
    fn timestamp_accessor(&self, args: &[Value], component: TimestampComponent) -> Value {
        match args.len() {
            1 => {
                // UTC version
                match &args[0] {
                    Value::Timestamp(ts) => {
                        if let Some(dt) = ts.to_datetime_utc() {
                            Value::Int(component.extract(&dt))
                        } else {
                            Value::error(EvalError::range_error("invalid timestamp"))
                        }
                    }
                    _ => Value::error(EvalError::no_matching_overload(&format!(
                        "get{}",
                        component_name(component)
                    ))),
                }
            }
            2 => {
                // Timezone version
                match (&args[0], &args[1]) {
                    (Value::Timestamp(ts), Value::String(tz_str)) => {
                        match time::parse_timezone(tz_str) {
                            Ok(tz_info) => {
                                if let Some(dt) = tz_info.datetime_from_timestamp(ts) {
                                    Value::Int(component.extract(&dt))
                                } else {
                                    Value::error(EvalError::range_error("invalid timestamp"))
                                }
                            }
                            Err(e) => Value::error(EvalError::invalid_argument(e)),
                        }
                    }
                    _ => Value::error(EvalError::no_matching_overload(&format!(
                        "get{}",
                        component_name(component)
                    ))),
                }
            }
            _ => Value::error(EvalError::no_matching_overload(&format!(
                "get{}",
                component_name(component)
            ))),
        }
    }

    /// Handle getHours - works on both Timestamp and Duration
    fn time_accessor_hours(&self, args: &[Value]) -> Value {
        match args.len() {
            1 => match &args[0] {
                Value::Timestamp(ts) => {
                    if let Some(dt) = ts.to_datetime_utc() {
                        Value::Int(TimestampComponent::Hours.extract(&dt))
                    } else {
                        Value::error(EvalError::range_error("invalid timestamp"))
                    }
                }
                Value::Duration(d) => Value::Int(d.get_hours()),
                _ => Value::error(EvalError::no_matching_overload("getHours")),
            },
            2 => {
                // Timestamp with timezone
                match (&args[0], &args[1]) {
                    (Value::Timestamp(ts), Value::String(tz_str)) => {
                        match time::parse_timezone(tz_str) {
                            Ok(tz_info) => {
                                if let Some(dt) = tz_info.datetime_from_timestamp(ts) {
                                    Value::Int(TimestampComponent::Hours.extract(&dt))
                                } else {
                                    Value::error(EvalError::range_error("invalid timestamp"))
                                }
                            }
                            Err(e) => Value::error(EvalError::invalid_argument(e)),
                        }
                    }
                    _ => Value::error(EvalError::no_matching_overload("getHours")),
                }
            }
            _ => Value::error(EvalError::no_matching_overload("getHours")),
        }
    }

    /// Handle getMinutes - works on both Timestamp and Duration
    fn time_accessor_minutes(&self, args: &[Value]) -> Value {
        match args.len() {
            1 => match &args[0] {
                Value::Timestamp(ts) => {
                    if let Some(dt) = ts.to_datetime_utc() {
                        Value::Int(TimestampComponent::Minutes.extract(&dt))
                    } else {
                        Value::error(EvalError::range_error("invalid timestamp"))
                    }
                }
                Value::Duration(d) => Value::Int(d.get_minutes()),
                _ => Value::error(EvalError::no_matching_overload("getMinutes")),
            },
            2 => {
                // Timestamp with timezone
                match (&args[0], &args[1]) {
                    (Value::Timestamp(ts), Value::String(tz_str)) => {
                        match time::parse_timezone(tz_str) {
                            Ok(tz_info) => {
                                if let Some(dt) = tz_info.datetime_from_timestamp(ts) {
                                    Value::Int(TimestampComponent::Minutes.extract(&dt))
                                } else {
                                    Value::error(EvalError::range_error("invalid timestamp"))
                                }
                            }
                            Err(e) => Value::error(EvalError::invalid_argument(e)),
                        }
                    }
                    _ => Value::error(EvalError::no_matching_overload("getMinutes")),
                }
            }
            _ => Value::error(EvalError::no_matching_overload("getMinutes")),
        }
    }

    /// Handle getSeconds - works on both Timestamp and Duration
    fn time_accessor_seconds(&self, args: &[Value]) -> Value {
        match args.len() {
            1 => match &args[0] {
                Value::Timestamp(ts) => {
                    if let Some(dt) = ts.to_datetime_utc() {
                        Value::Int(TimestampComponent::Seconds.extract(&dt))
                    } else {
                        Value::error(EvalError::range_error("invalid timestamp"))
                    }
                }
                Value::Duration(d) => Value::Int(d.total_seconds()),
                _ => Value::error(EvalError::no_matching_overload("getSeconds")),
            },
            2 => {
                // Timestamp with timezone
                match (&args[0], &args[1]) {
                    (Value::Timestamp(ts), Value::String(tz_str)) => {
                        match time::parse_timezone(tz_str) {
                            Ok(tz_info) => {
                                if let Some(dt) = tz_info.datetime_from_timestamp(ts) {
                                    Value::Int(TimestampComponent::Seconds.extract(&dt))
                                } else {
                                    Value::error(EvalError::range_error("invalid timestamp"))
                                }
                            }
                            Err(e) => Value::error(EvalError::invalid_argument(e)),
                        }
                    }
                    _ => Value::error(EvalError::no_matching_overload("getSeconds")),
                }
            }
            _ => Value::error(EvalError::no_matching_overload("getSeconds")),
        }
    }

    /// Handle getMilliseconds - works on both Timestamp and Duration
    fn time_accessor_milliseconds(&self, args: &[Value]) -> Value {
        match args.len() {
            1 => match &args[0] {
                Value::Timestamp(ts) => {
                    if let Some(dt) = ts.to_datetime_utc() {
                        Value::Int(TimestampComponent::Milliseconds.extract(&dt))
                    } else {
                        Value::error(EvalError::range_error("invalid timestamp"))
                    }
                }
                Value::Duration(d) => Value::Int(d.get_milliseconds()),
                _ => Value::error(EvalError::no_matching_overload("getMilliseconds")),
            },
            2 => {
                // Timestamp with timezone
                match (&args[0], &args[1]) {
                    (Value::Timestamp(ts), Value::String(tz_str)) => {
                        match time::parse_timezone(tz_str) {
                            Ok(tz_info) => {
                                if let Some(dt) = tz_info.datetime_from_timestamp(ts) {
                                    Value::Int(TimestampComponent::Milliseconds.extract(&dt))
                                } else {
                                    Value::error(EvalError::range_error("invalid timestamp"))
                                }
                            }
                            Err(e) => Value::error(EvalError::invalid_argument(e)),
                        }
                    }
                    _ => Value::error(EvalError::no_matching_overload("getMilliseconds")),
                }
            }
            _ => Value::error(EvalError::no_matching_overload("getMilliseconds")),
        }
    }

    fn builtin_size(&self, value: &Value) -> Value {
        match value {
            Value::String(s) => Value::Int(s.chars().count() as i64),
            Value::Bytes(b) => Value::Int(b.len() as i64),
            Value::List(l) => Value::Int(l.len() as i64),
            Value::Map(m) => Value::Int(m.len() as i64),
            _ => Value::error(EvalError::no_matching_overload("size")),
        }
    }

    fn builtin_contains(&self, receiver: &Value, arg: &Value) -> Value {
        match (receiver, arg) {
            (Value::String(s), Value::String(sub)) => Value::Bool(s.contains(sub.as_ref())),
            _ => Value::error(EvalError::no_matching_overload("contains")),
        }
    }

    fn builtin_starts_with(&self, receiver: &Value, arg: &Value) -> Value {
        match (receiver, arg) {
            (Value::String(s), Value::String(prefix)) => {
                Value::Bool(s.starts_with(prefix.as_ref()))
            }
            _ => Value::error(EvalError::no_matching_overload("startsWith")),
        }
    }

    fn builtin_ends_with(&self, receiver: &Value, arg: &Value) -> Value {
        match (receiver, arg) {
            (Value::String(s), Value::String(suffix)) => Value::Bool(s.ends_with(suffix.as_ref())),
            _ => Value::error(EvalError::no_matching_overload("endsWith")),
        }
    }

    fn builtin_matches(&self, receiver: &Value, arg: &Value) -> Value {
        match (receiver, arg) {
            (Value::String(s), Value::String(pattern)) => {
                match regex::Regex::new(pattern.as_ref()) {
                    Ok(re) => Value::Bool(re.is_match(s.as_ref())),
                    Err(e) => Value::error(EvalError::invalid_argument(format!(
                        "invalid regex: {}",
                        e
                    ))),
                }
            }
            _ => Value::error(EvalError::no_matching_overload("matches")),
        }
    }

    fn eval_struct(
        &self,
        type_name: &SpannedExpr,
        fields: &[crate::types::StructField],
    ) -> Value {
        // Get the fully qualified type name
        let extracted_name = self.get_type_name_from_expr(type_name);
        let fq_name = match self.resolve_type_name(type_name) {
            Some(name) => name,
            None => {
                return Value::error(EvalError::internal(format!(
                    "could not resolve type name for struct (extracted: {:?})",
                    extracted_name
                )))
            }
        };

        // Get the proto type registry
        let registry = match self.proto_types {
            Some(r) => r,
            None => {
                return Value::error(EvalError::internal(format!(
                    "proto type registry not available for struct construction (type: {})",
                    fq_name
                )))
            }
        };

        // Get the message descriptor
        let descriptor = match registry.get_message(&fq_name) {
            Some(d) => d,
            None => {
                return Value::error(EvalError::internal(format!(
                    "unknown message type: {} (registry has proto_types: true)",
                    fq_name
                )))
            }
        };

        // Shortcut for wrapper types: bypass DynamicMessage field-setting
        if wkt::is_wrapper_type(&descriptor) {
            return self.eval_wrapper_struct(&descriptor, fields);
        }

        // Shortcut for google.protobuf.Any: construct directly
        if descriptor.full_name() == "google.protobuf.Any"
            || fq_name == "google.protobuf.Any"
        {
            return self.eval_any_struct(&descriptor, fields);
        }

        // Create the dynamic message
        let mut message = DynamicMessage::new(descriptor.clone());

        // Set each field
        for field in fields {
            let value = self.eval_expr(&field.value);
            if value.is_error() {
                return value;
            }

            // Handle optional fields - only set if value is present
            if field.optional {
                match value {
                    Value::Optional(OptionalValue::Some(v)) => {
                        if let Err(e) = self.set_proto_field_or_null(&mut message, &field.name, *v) {
                            return e;
                        }
                    }
                    Value::Optional(OptionalValue::None) => {
                        // Skip absent optionals
                    }
                    _ => {
                        if let Err(e) = self.set_proto_field_or_null(&mut message, &field.name, value) {
                            return e;
                        }
                    }
                }
            } else {
                if let Err(e) = self.set_proto_field_or_null(&mut message, &field.name, value) {
                    return e;
                }
            }
        }

        // Check for well-known type unwrapping
        self.maybe_unwrap_well_known(message)
    }

    /// Construct a wrapper type (e.g., google.protobuf.Int32Value{value: 1}).
    /// Bypasses the normal DynamicMessage field-setting path.
    fn eval_wrapper_struct(
        &self,
        descriptor: &MessageDescriptor,
        fields: &[crate::types::StructField],
    ) -> Value {
        // Empty wrapper construction returns the default value
        if fields.is_empty() {
            return match descriptor.full_name() {
                "google.protobuf.BoolValue" => Value::Bool(false),
                "google.protobuf.Int32Value" | "google.protobuf.Int64Value" => Value::Int(0),
                "google.protobuf.UInt32Value" | "google.protobuf.UInt64Value" => Value::UInt(0),
                "google.protobuf.FloatValue" | "google.protobuf.DoubleValue" => Value::Double(0.0),
                "google.protobuf.StringValue" => Value::String(Arc::from("")),
                "google.protobuf.BytesValue" => Value::Bytes(Arc::from(Vec::<u8>::new().as_slice())),
                _ => Value::error(EvalError::internal("unknown wrapper type")),
            };
        }

        // Wrapper types have a single field named "value"
        if fields.len() != 1 || fields[0].name != "value" {
            return Value::error(EvalError::internal(
                "wrapper type expects exactly one field named 'value'",
            ));
        }

        let value = self.eval_expr(&fields[0].value);
        if value.is_error() {
            return value;
        }

        // Convert the value directly based on wrapper type
        let type_name = descriptor.full_name();
        match type_name {
            "google.protobuf.BoolValue" => match &value {
                Value::Bool(_) => value,
                _ => Value::error(EvalError::type_mismatch("bool", &value.cel_type().display_name())),
            },
            "google.protobuf.Int32Value" => match &value {
                Value::Int(i) => {
                    if *i < i32::MIN as i64 || *i > i32::MAX as i64 {
                        Value::error(EvalError::overflow("int to int32 overflow"))
                    } else {
                        Value::Int(*i)
                    }
                }
                Value::UInt(u) => {
                    if *u > i32::MAX as u64 {
                        Value::error(EvalError::overflow("uint to int32 overflow"))
                    } else {
                        Value::Int(*u as i64)
                    }
                }
                Value::Double(d) => {
                    if d.fract() != 0.0 || *d < i32::MIN as f64 || *d > i32::MAX as f64 {
                        Value::error(EvalError::overflow("double to int32 overflow"))
                    } else {
                        Value::Int(*d as i64)
                    }
                }
                _ => Value::error(EvalError::type_mismatch("int", &value.cel_type().display_name())),
            },
            "google.protobuf.Int64Value" => match &value {
                Value::Int(_) => value,
                Value::UInt(u) => {
                    if *u > i64::MAX as u64 {
                        Value::error(EvalError::overflow("uint to int64 overflow"))
                    } else {
                        Value::Int(*u as i64)
                    }
                }
                _ => Value::error(EvalError::type_mismatch("int", &value.cel_type().display_name())),
            },
            "google.protobuf.UInt32Value" => match &value {
                Value::UInt(u) => {
                    if *u > u32::MAX as u64 {
                        Value::error(EvalError::overflow("uint to uint32 overflow"))
                    } else {
                        Value::UInt(*u)
                    }
                }
                Value::Int(i) => {
                    if *i < 0 || *i > u32::MAX as i64 {
                        Value::error(EvalError::overflow("int to uint32 overflow"))
                    } else {
                        Value::UInt(*i as u64)
                    }
                }
                _ => Value::error(EvalError::type_mismatch("uint", &value.cel_type().display_name())),
            },
            "google.protobuf.UInt64Value" => match &value {
                Value::UInt(_) => value,
                Value::Int(i) => {
                    if *i < 0 {
                        Value::error(EvalError::overflow("negative int to uint64"))
                    } else {
                        Value::UInt(*i as u64)
                    }
                }
                _ => Value::error(EvalError::type_mismatch("uint", &value.cel_type().display_name())),
            },
            "google.protobuf.FloatValue" | "google.protobuf.DoubleValue" => match &value {
                Value::Double(_) => value,
                Value::Int(i) => Value::Double(*i as f64),
                Value::UInt(u) => Value::Double(*u as f64),
                _ => Value::error(EvalError::type_mismatch("double", &value.cel_type().display_name())),
            },
            "google.protobuf.StringValue" => match &value {
                Value::String(_) => value,
                _ => Value::error(EvalError::type_mismatch("string", &value.cel_type().display_name())),
            },
            "google.protobuf.BytesValue" => match &value {
                Value::Bytes(_) => value,
                _ => Value::error(EvalError::type_mismatch("bytes", &value.cel_type().display_name())),
            },
            _ => Value::error(EvalError::internal(format!(
                "unknown wrapper type: {}",
                type_name
            ))),
        }
    }

    /// Construct a google.protobuf.Any message directly.
    /// Works around potential descriptor pool issues with the Any type.
    fn eval_any_struct(
        &self,
        descriptor: &MessageDescriptor,
        fields: &[crate::types::StructField],
    ) -> Value {
        let mut type_url = String::new();
        let mut value_bytes = Vec::new();

        for field in fields {
            let val = self.eval_expr(&field.value);
            if val.is_error() {
                return val;
            }
            match field.name.as_str() {
                "type_url" => {
                    if let Value::String(s) = &val {
                        type_url = s.to_string();
                    } else {
                        return Value::error(EvalError::type_mismatch(
                            "string",
                            &val.cel_type().display_name(),
                        ));
                    }
                }
                "value" => {
                    if let Value::Bytes(b) = &val {
                        value_bytes = b.to_vec();
                    } else {
                        return Value::error(EvalError::type_mismatch(
                            "bytes",
                            &val.cel_type().display_name(),
                        ));
                    }
                }
                other => {
                    return Value::error(EvalError::field_not_found(other));
                }
            }
        }

        // Build the Any message by setting fields directly
        let mut msg = DynamicMessage::new(descriptor.clone());
        if let Some(type_url_field) = descriptor.get_field_by_name("type_url") {
            msg.set_field(
                &type_url_field,
                prost_reflect::Value::String(type_url.clone()),
            );
        } else {
            // Fallback: try to find by field number (1 for type_url, 2 for value)
            for field_desc in descriptor.fields() {
                match field_desc.number() {
                    1 => msg.set_field(
                        &field_desc,
                        prost_reflect::Value::String(type_url.clone()),
                    ),
                    2 => msg.set_field(
                        &field_desc,
                        prost_reflect::Value::Bytes(prost::bytes::Bytes::from(
                            value_bytes.clone(),
                        )),
                    ),
                    _ => {}
                }
            }
        }
        if let Some(value_field) = descriptor.get_field_by_name("value") {
            msg.set_field(
                &value_field,
                prost_reflect::Value::Bytes(prost::bytes::Bytes::from(value_bytes)),
            );
        }

        Value::Proto(super::ProtoValue::new(msg))
    }

    /// Resolve a type name expression to a fully qualified name.
    ///
    /// Uses the following resolution order:
    /// 1. Reference map (pre-resolved from type checking)
    /// 2. Abbreviations expansion
    /// 3. Container-based resolution (C++ namespace rules)
    /// 4. Return the unresolved name as last resort
    fn resolve_type_name(&self, expr: &SpannedExpr) -> Option<String> {
        // First, check the reference_map for pre-resolved names
        if let Some(ref_map) = self.reference_map {
            if let Some(ref_info) = ref_map.get(&expr.id) {
                return Some(ref_info.name.clone());
            }
        }

        // Fall back to extracting the name from the expression
        let name = self.get_type_name_from_expr(expr)?;

        // Try to expand abbreviations
        let expanded_name = self.expand_abbreviation(&name);
        let name_to_resolve = expanded_name.as_ref().unwrap_or(&name);

        // If we have a proto registry, try to resolve using container
        if let Some(registry) = self.proto_types {
            // Use container-based resolution (C++ namespace rules)
            if let Some(fq_name) = registry.resolve_message_name(name_to_resolve, &self.container) {
                return Some(fq_name);
            }
        }

        // Return the unresolved name as a last resort
        Some(name_to_resolve.clone())
    }

    /// Try to expand a name using abbreviations.
    ///
    /// If the first segment of the name matches an abbreviation, the full
    /// qualified name is substituted.
    fn expand_abbreviation(&self, name: &str) -> Option<String> {
        let abbrevs = self.abbreviations?;

        // Get the first segment of the name
        let first_segment = name.split('.').next()?;

        // Check if it's an abbreviation
        if let Some(qualified) = abbrevs.get(first_segment) {
            // If the name has more segments, append them to the expanded name
            if let Some(rest) = name.strip_prefix(first_segment).and_then(|s| s.strip_prefix('.')) {
                Some(format!("{}.{}", qualified, rest))
            } else {
                Some(qualified.clone())
            }
        } else {
            None
        }
    }

    /// Extract a type name from an expression (for unresolved cases).
    fn get_type_name_from_expr(&self, expr: &SpannedExpr) -> Option<String> {
        match &expr.node {
            Expr::Ident(name) => Some(name.clone()),
            Expr::RootIdent(name) => Some(name.clone()),
            Expr::Member {
                expr: inner,
                field,
                ..
            } => {
                let prefix = self.get_type_name_from_expr(inner)?;
                Some(format!("{}.{}", prefix, field))
            }
            _ => None,
        }
    }

    /// Set a field on a proto message, handling null values.
    /// Null is valid only for singular message-type fields (means "absent").
    /// For scalar, repeated, or map fields, null produces an error.
    fn set_proto_field_or_null(
        &self,
        message: &mut DynamicMessage,
        field_name: &str,
        value: Value,
    ) -> Result<(), Value> {
        if matches!(value, Value::Null) {
            let descriptor = message.descriptor();
            let field = match descriptor.get_field_by_name(field_name) {
                Some(f) => f,
                None => return Err(Value::error(EvalError::field_not_found(field_name))),
            };
            // Null is only valid for singular message fields (except ListValue/Struct
            // which map to list/map types, not nullable messages)
            if !field.is_list() && !field.is_map() {
                if let Kind::Message(msg_desc) = field.kind() {
                    let msg_name = msg_desc.full_name();
                    if msg_name == "google.protobuf.ListValue"
                        || msg_name == "google.protobuf.Struct"
                    {
                        return Err(Value::error(EvalError::type_mismatch(
                            &format!("{} field", msg_name),
                            "null",
                        )));
                    }
                    // For google.protobuf.Value, null sets the null_value oneof
                    if msg_name == "google.protobuf.Value" {
                        if let Some(registry) = self.proto_types {
                            let val_msg =
                                wkt::cel_value_to_google_value(&Value::Null, registry)?;
                            message.set_field(&field, prost_reflect::Value::Message(val_msg));
                            return Ok(());
                        }
                    }
                    // Leave the field unset (absent) — don't call set_field
                    return Ok(());
                }
            }
            return Err(Value::error(EvalError::type_mismatch(
                "unsupported field type",
                "null",
            )));
        }
        self.set_proto_field(message, field_name, value)
    }

    /// Set a field on a proto message.
    fn set_proto_field(
        &self,
        message: &mut DynamicMessage,
        field_name: &str,
        value: Value,
    ) -> Result<(), Value> {
        let descriptor = message.descriptor();
        let field = match descriptor.get_field_by_name(field_name) {
            Some(f) => f,
            None => {
                // Try extension field lookup
                if let Some(registry) = self.proto_types {
                    if let Some(ext) = registry.get_extension_by_name(field_name) {
                        if ext.containing_message() == descriptor {
                            let proto_value =
                                self.scalar_value_to_proto(&value, &ext.kind())?;
                            message.set_extension(&ext, proto_value);
                            return Ok(());
                        }
                    }
                }
                return Err(Value::error(EvalError::field_not_found(field_name)));
            }
        };

        let proto_value = self.value_to_proto_reflect(&value, &field)?;
        message.set_field(&field, proto_value);
        Ok(())
    }

    /// Convert a CEL Value to a prost_reflect Value for setting proto fields.
    fn value_to_proto_reflect(
        &self,
        value: &Value,
        field: &FieldDescriptor,
    ) -> Result<prost_reflect::Value, Value> {
        // Handle repeated fields
        if field.is_list() {
            match value {
                Value::List(list) => {
                    let mut values = Vec::with_capacity(list.len());
                    for item in list.iter() {
                        values.push(self.scalar_value_to_proto(item, &field.kind())?);
                    }
                    return Ok(prost_reflect::Value::List(values));
                }
                _ => {
                    return Err(Value::error(EvalError::type_mismatch(
                        "list",
                        &value.cel_type().display_name(),
                    )));
                }
            }
        }

        // Handle map fields
        if field.is_map() {
            match value {
                Value::Map(map) => {
                    let mut proto_map = HashMap::new();
                    if let Kind::Message(map_entry) = field.kind() {
                        let key_field = map_entry.get_field_by_name("key");
                        let value_field = map_entry.get_field_by_name("value");
                        if let (Some(kf), Some(vf)) = (key_field, value_field) {
                            for (k, v) in map.iter() {
                                let proto_key = self.map_key_to_proto(k, &kf.kind())?;
                                let proto_val = self.scalar_value_to_proto(v, &vf.kind())?;
                                proto_map.insert(proto_key, proto_val);
                            }
                        }
                    }
                    return Ok(prost_reflect::Value::Map(proto_map));
                }
                _ => {
                    return Err(Value::error(EvalError::type_mismatch(
                        "map",
                        &value.cel_type().display_name(),
                    )));
                }
            }
        }

        // Scalar value
        self.scalar_value_to_proto(value, &field.kind())
    }

    /// Convert a scalar CEL Value to a prost_reflect Value.
    fn scalar_value_to_proto(
        &self,
        value: &Value,
        kind: &Kind,
    ) -> Result<prost_reflect::Value, Value> {
        match (value, kind) {
            (Value::Bool(b), Kind::Bool) => Ok(prost_reflect::Value::Bool(*b)),
            (Value::Int(i), Kind::Int32 | Kind::Sint32 | Kind::Sfixed32) => {
                if *i < i32::MIN as i64 || *i > i32::MAX as i64 {
                    Err(Value::error(EvalError::overflow("int to int32 overflow")))
                } else {
                    Ok(prost_reflect::Value::I32(*i as i32))
                }
            }
            (Value::Int(i), Kind::Int64 | Kind::Sint64 | Kind::Sfixed64) => {
                Ok(prost_reflect::Value::I64(*i))
            }
            (Value::Int(i), Kind::Enum(_)) => Ok(prost_reflect::Value::EnumNumber(*i as i32)),
            (Value::UInt(u), Kind::Uint32 | Kind::Fixed32) => {
                if *u > u32::MAX as u64 {
                    Err(Value::error(EvalError::overflow("uint to uint32 overflow")))
                } else {
                    Ok(prost_reflect::Value::U32(*u as u32))
                }
            }
            (Value::UInt(u), Kind::Uint64 | Kind::Fixed64) => Ok(prost_reflect::Value::U64(*u)),
            (Value::Double(d), Kind::Double) => Ok(prost_reflect::Value::F64(*d)),
            (Value::Double(d), Kind::Float) => Ok(prost_reflect::Value::F32(*d as f32)),
            (Value::String(s), Kind::String) => Ok(prost_reflect::Value::String(s.to_string())),
            (Value::Bytes(b), Kind::Bytes) => {
                Ok(prost_reflect::Value::Bytes(prost::bytes::Bytes::copy_from_slice(b)))
            }
            // Cross-type numeric coercion: Int → Uint
            (Value::Int(i), Kind::Uint32 | Kind::Fixed32) => {
                if *i < 0 || *i > u32::MAX as i64 {
                    Err(Value::error(EvalError::overflow("int to uint32 overflow")))
                } else {
                    Ok(prost_reflect::Value::U32(*i as u32))
                }
            }
            (Value::Int(i), Kind::Uint64 | Kind::Fixed64) => {
                if *i < 0 {
                    Err(Value::error(EvalError::overflow("negative int to uint64")))
                } else {
                    Ok(prost_reflect::Value::U64(*i as u64))
                }
            }
            // Cross-type numeric coercion: UInt → Int
            (Value::UInt(u), Kind::Int32 | Kind::Sint32 | Kind::Sfixed32) => {
                if *u > i32::MAX as u64 {
                    Err(Value::error(EvalError::overflow("uint to int32 overflow")))
                } else {
                    Ok(prost_reflect::Value::I32(*u as i32))
                }
            }
            (Value::UInt(u), Kind::Int64 | Kind::Sint64 | Kind::Sfixed64) => {
                if *u > i64::MAX as u64 {
                    Err(Value::error(EvalError::overflow("uint to int64 overflow")))
                } else {
                    Ok(prost_reflect::Value::I64(*u as i64))
                }
            }
            // Cross-type numeric coercion: Int/UInt → Double/Float
            (Value::Int(i), Kind::Double) => Ok(prost_reflect::Value::F64(*i as f64)),
            (Value::Int(i), Kind::Float) => Ok(prost_reflect::Value::F32(*i as f32)),
            (Value::UInt(u), Kind::Double) => Ok(prost_reflect::Value::F64(*u as f64)),
            (Value::UInt(u), Kind::Float) => Ok(prost_reflect::Value::F32(*u as f32)),
            // Cross-type numeric coercion: Double → Int/UInt (check integrality)
            (Value::Double(d), Kind::Int32 | Kind::Sint32 | Kind::Sfixed32) => {
                if d.fract() != 0.0 || *d < i32::MIN as f64 || *d > i32::MAX as f64 {
                    Err(Value::error(EvalError::overflow("double to int32 overflow")))
                } else {
                    Ok(prost_reflect::Value::I32(*d as i32))
                }
            }
            (Value::Double(d), Kind::Int64 | Kind::Sint64 | Kind::Sfixed64) => {
                if d.fract() != 0.0 || *d < i64::MIN as f64 || *d > i64::MAX as f64 {
                    Err(Value::error(EvalError::overflow("double to int64 overflow")))
                } else {
                    Ok(prost_reflect::Value::I64(*d as i64))
                }
            }
            (Value::Double(d), Kind::Uint32 | Kind::Fixed32) => {
                if d.fract() != 0.0 || *d < 0.0 || *d > u32::MAX as f64 {
                    Err(Value::error(EvalError::overflow("double to uint32 overflow")))
                } else {
                    Ok(prost_reflect::Value::U32(*d as u32))
                }
            }
            (Value::Double(d), Kind::Uint64 | Kind::Fixed64) => {
                if d.fract() != 0.0 || *d < 0.0 || *d > u64::MAX as f64 {
                    Err(Value::error(EvalError::overflow("double to uint64 overflow")))
                } else {
                    Ok(prost_reflect::Value::U64(*d as u64))
                }
            }
            // Handle Timestamp -> google.protobuf.Timestamp conversion
            (Value::Timestamp(ts), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.Timestamp" =>
            {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(seconds_field) = msg_desc.get_field_by_name("seconds") {
                    msg.set_field(&seconds_field, prost_reflect::Value::I64(ts.seconds));
                }
                if let Some(nanos_field) = msg_desc.get_field_by_name("nanos") {
                    msg.set_field(&nanos_field, prost_reflect::Value::I32(ts.nanos));
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            // Handle Duration -> google.protobuf.Duration conversion
            (Value::Duration(d), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.Duration" =>
            {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(seconds_field) = msg_desc.get_field_by_name("seconds") {
                    msg.set_field(&seconds_field, prost_reflect::Value::I64(d.seconds));
                }
                if let Some(nanos_field) = msg_desc.get_field_by_name("nanos") {
                    msg.set_field(&nanos_field, prost_reflect::Value::I32(d.nanos));
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            // Handle wrapper types
            (Value::Int(i), Kind::Message(msg_desc)) if wkt::is_int_wrapper(msg_desc) => {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    let proto_val = if msg_desc.full_name() == "google.protobuf.Int32Value" {
                        if *i < i32::MIN as i64 || *i > i32::MAX as i64 {
                            return Err(Value::error(EvalError::overflow("int to int32 overflow")));
                        }
                        prost_reflect::Value::I32(*i as i32)
                    } else {
                        prost_reflect::Value::I64(*i)
                    };
                    msg.set_field(&value_field, proto_val);
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            (Value::UInt(u), Kind::Message(msg_desc)) if wkt::is_uint_wrapper(msg_desc) => {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    let proto_val = if msg_desc.full_name() == "google.protobuf.UInt32Value" {
                        if *u > u32::MAX as u64 {
                            return Err(Value::error(EvalError::overflow("uint to uint32 overflow")));
                        }
                        prost_reflect::Value::U32(*u as u32)
                    } else {
                        prost_reflect::Value::U64(*u)
                    };
                    msg.set_field(&value_field, proto_val);
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            (Value::Double(d), Kind::Message(msg_desc)) if wkt::is_double_wrapper(msg_desc) => {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    let proto_val = if msg_desc.full_name() == "google.protobuf.FloatValue" {
                        prost_reflect::Value::F32(*d as f32)
                    } else {
                        prost_reflect::Value::F64(*d)
                    };
                    msg.set_field(&value_field, proto_val);
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            (Value::Bool(b), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.BoolValue" =>
            {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    msg.set_field(&value_field, prost_reflect::Value::Bool(*b));
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            (Value::String(s), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.StringValue" =>
            {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    msg.set_field(&value_field, prost_reflect::Value::String(s.to_string()));
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            (Value::Bytes(b), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.BytesValue" =>
            {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    msg.set_field(
                        &value_field,
                        prost_reflect::Value::Bytes(prost::bytes::Bytes::copy_from_slice(b)),
                    );
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            // Cross-type numeric coercion for wrapper types: Int → UInt wrapper
            (Value::Int(i), Kind::Message(msg_desc)) if wkt::is_uint_wrapper(msg_desc) => {
                if *i < 0 {
                    return Err(Value::error(EvalError::overflow("negative int to uint wrapper")));
                }
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    let proto_val = if msg_desc.full_name() == "google.protobuf.UInt32Value" {
                        if *i > u32::MAX as i64 {
                            return Err(Value::error(EvalError::overflow("int to uint32 overflow")));
                        }
                        prost_reflect::Value::U32(*i as u32)
                    } else {
                        prost_reflect::Value::U64(*i as u64)
                    };
                    msg.set_field(&value_field, proto_val);
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            // Cross-type numeric coercion for wrapper types: UInt → Int wrapper
            (Value::UInt(u), Kind::Message(msg_desc)) if wkt::is_int_wrapper(msg_desc) => {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    let proto_val = if msg_desc.full_name() == "google.protobuf.Int32Value" {
                        if *u > i32::MAX as u64 {
                            return Err(Value::error(EvalError::overflow("uint to int32 overflow")));
                        }
                        prost_reflect::Value::I32(*u as i32)
                    } else {
                        if *u > i64::MAX as u64 {
                            return Err(Value::error(EvalError::overflow("uint to int64 overflow")));
                        }
                        prost_reflect::Value::I64(*u as i64)
                    };
                    msg.set_field(&value_field, proto_val);
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            // Cross-type numeric coercion for wrapper types: Int/UInt → Double wrapper
            (Value::Int(i), Kind::Message(msg_desc)) if wkt::is_double_wrapper(msg_desc) => {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    let proto_val = if msg_desc.full_name() == "google.protobuf.FloatValue" {
                        prost_reflect::Value::F32(*i as f32)
                    } else {
                        prost_reflect::Value::F64(*i as f64)
                    };
                    msg.set_field(&value_field, proto_val);
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            (Value::UInt(u), Kind::Message(msg_desc)) if wkt::is_double_wrapper(msg_desc) => {
                let mut msg = DynamicMessage::new(msg_desc.clone());
                if let Some(value_field) = msg_desc.get_field_by_name("value") {
                    let proto_val = if msg_desc.full_name() == "google.protobuf.FloatValue" {
                        prost_reflect::Value::F32(*u as f32)
                    } else {
                        prost_reflect::Value::F64(*u as f64)
                    };
                    msg.set_field(&value_field, proto_val);
                }
                Ok(prost_reflect::Value::Message(msg))
            }
            // google.protobuf.Value coercion
            (_, Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.Value" =>
            {
                if let Some(registry) = self.proto_types {
                    let msg = wkt::cel_value_to_google_value(value, registry)?;
                    Ok(prost_reflect::Value::Message(msg))
                } else {
                    Err(Value::error(EvalError::internal("proto type registry not available for Value coercion")))
                }
            }
            // google.protobuf.Struct coercion
            (Value::Map(map), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.Struct" =>
            {
                if let Some(registry) = self.proto_types {
                    let msg = wkt::cel_map_to_struct(map, registry)?;
                    Ok(prost_reflect::Value::Message(msg))
                } else {
                    Err(Value::error(EvalError::internal("proto type registry not available for Struct coercion")))
                }
            }
            // google.protobuf.ListValue coercion
            (Value::List(list), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.ListValue" =>
            {
                if let Some(registry) = self.proto_types {
                    let msg = wkt::cel_list_to_list_value(list, registry)?;
                    Ok(prost_reflect::Value::Message(msg))
                } else {
                    Err(Value::error(EvalError::internal("proto type registry not available for ListValue coercion")))
                }
            }
            // google.protobuf.Any packing for proto messages
            (Value::Proto(proto), Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.Any" =>
            {
                wkt::pack_message_into_any(proto.message(), msg_desc)
            }
            // google.protobuf.Any wrapping for primitive values
            (_, Kind::Message(msg_desc))
                if msg_desc.full_name() == "google.protobuf.Any" =>
            {
                if let Some(registry) = self.proto_types {
                    let any_msg = wkt::wrap_value_for_any(value, registry)?;
                    Ok(prost_reflect::Value::Message(any_msg))
                } else {
                    Err(Value::error(EvalError::internal("proto type registry not available for Any wrapping")))
                }
            }
            (Value::Null, Kind::Message(msg_desc)) => {
                // Null for message fields - create default message
                Ok(prost_reflect::Value::Message(DynamicMessage::new(msg_desc.clone())))
            }
            (Value::Proto(proto), Kind::Message(_)) => {
                Ok(prost_reflect::Value::Message((*proto.message()).clone()))
            }
            _ => Err(Value::error(EvalError::type_mismatch(
                &format!("{:?}", kind),
                &value.cel_type().display_name(),
            ))),
        }
    }

    /// Convert a CEL MapKey to a prost_reflect MapKey.
    fn map_key_to_proto(
        &self,
        key: &MapKey,
        kind: &Kind,
    ) -> Result<ProtoMapKey, Value> {
        match (key, kind) {
            (MapKey::Bool(b), Kind::Bool) => Ok(ProtoMapKey::Bool(*b)),
            (MapKey::Int(i), Kind::Int32 | Kind::Sint32 | Kind::Sfixed32) => {
                Ok(ProtoMapKey::I32(*i as i32))
            }
            (MapKey::Int(i), Kind::Int64 | Kind::Sint64 | Kind::Sfixed64) => {
                Ok(ProtoMapKey::I64(*i))
            }
            (MapKey::UInt(u), Kind::Uint32 | Kind::Fixed32) => Ok(ProtoMapKey::U32(*u as u32)),
            (MapKey::UInt(u), Kind::Uint64 | Kind::Fixed64) => Ok(ProtoMapKey::U64(*u)),
            (MapKey::String(s), Kind::String) => Ok(ProtoMapKey::String(s.to_string())),
            _ => Err(Value::error(EvalError::type_mismatch(
                &format!("{:?}", kind),
                &format!("{:?}", key),
            ))),
        }
    }

    /// Check if a message is a well-known type and unwrap it to a native CEL value.
    fn maybe_unwrap_well_known(&self, message: DynamicMessage) -> Value {
        // Handle google.protobuf.Any: unpack and then unwrap the inner message
        if message.descriptor().full_name() == "google.protobuf.Any" {
            return self.unpack_any(&message);
        }
        wkt::maybe_unwrap_well_known(message)
    }

    /// Unpack a `google.protobuf.Any` message to a native CEL value.
    fn unpack_any(&self, any_msg: &DynamicMessage) -> Value {
        let type_url_field = match any_msg.descriptor().get_field_by_name("type_url") {
            Some(f) => f,
            None => return Value::Proto(super::ProtoValue::new(any_msg.clone())),
        };
        let type_url = match any_msg.get_field(&type_url_field).into_owned() {
            prost_reflect::Value::String(s) => s,
            _ => return Value::Proto(super::ProtoValue::new(any_msg.clone())),
        };

        if type_url.is_empty() {
            return Value::Proto(super::ProtoValue::new(any_msg.clone()));
        }

        let type_name = type_url
            .strip_prefix("type.googleapis.com/")
            .unwrap_or(&type_url);

        let registry = match self.proto_types {
            Some(r) => r,
            None => return Value::Proto(super::ProtoValue::new(any_msg.clone())),
        };

        let descriptor = match registry.get_message(type_name) {
            Some(d) => d,
            None => return Value::Proto(super::ProtoValue::new(any_msg.clone())),
        };

        let value_field = match any_msg.descriptor().get_field_by_name("value") {
            Some(f) => f,
            None => return Value::Proto(super::ProtoValue::new(any_msg.clone())),
        };
        let value_bytes = match any_msg.get_field(&value_field).into_owned() {
            prost_reflect::Value::Bytes(b) => b,
            _ => return Value::Proto(super::ProtoValue::new(any_msg.clone())),
        };

        match DynamicMessage::decode(descriptor.clone(), value_bytes.as_ref()) {
            Ok(inner_msg) => self.maybe_unwrap_well_known(inner_msg),
            Err(_) => Value::Proto(super::ProtoValue::new(any_msg.clone())),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn eval_comprehension(
        &self,
        iter_var: &str,
        iter_var2: &str,
        iter_range: &SpannedExpr,
        accu_var: &str,
        accu_init: &SpannedExpr,
        loop_condition: &SpannedExpr,
        loop_step: &SpannedExpr,
        result: &SpannedExpr,
    ) -> Value {
        // Evaluate the iteration range
        let range_val = self.eval_expr(iter_range);
        if range_val.is_error() {
            return range_val;
        }

        // Initialize accumulator
        let mut accu = self.eval_expr(accu_init);
        if accu.is_error() {
            return accu;
        }

        // Create iterator based on range type
        match &range_val {
            Value::List(list) => {
                for (i, elem) in list.iter().enumerate() {
                    // Create nested activation with iteration variables
                    let mut iter_activation =
                        HierarchicalActivation::new(self.activation).with_binding(accu_var, accu.clone());

                    if !iter_var.is_empty() {
                        iter_activation.insert(iter_var, elem.clone());
                    }
                    if !iter_var2.is_empty() {
                        iter_activation.insert(iter_var2, Value::Int(i as i64));
                    }

                    let iter_eval = self.child_evaluator(&iter_activation);

                    // Check loop condition
                    let cond = iter_eval.eval_expr(loop_condition);
                    match &cond {
                        Value::Bool(false) => break,
                        Value::Bool(true) => {}
                        Value::Error(_) => return cond,
                        _ => {
                            return Value::error(EvalError::type_mismatch(
                                "bool",
                                &cond.cel_type().display_name(),
                            ))
                        }
                    }

                    // Compute next accumulator value
                    accu = iter_eval.eval_expr(loop_step);
                    if accu.is_error() {
                        return accu;
                    }
                }
            }
            Value::Map(map) => {
                for (key, val) in map.iter() {
                    let mut iter_activation =
                        HierarchicalActivation::new(self.activation).with_binding(accu_var, accu.clone());

                    if !iter_var.is_empty() {
                        iter_activation.insert(iter_var, key.to_value());
                    }
                    if !iter_var2.is_empty() {
                        iter_activation.insert(iter_var2, val.clone());
                    }

                    let iter_eval = self.child_evaluator(&iter_activation);

                    // Check loop condition
                    let cond = iter_eval.eval_expr(loop_condition);
                    match &cond {
                        Value::Bool(false) => break,
                        Value::Bool(true) => {}
                        Value::Error(_) => return cond,
                        _ => {
                            return Value::error(EvalError::type_mismatch(
                                "bool",
                                &cond.cel_type().display_name(),
                            ))
                        }
                    }

                    // Compute next accumulator value
                    accu = iter_eval.eval_expr(loop_step);
                    if accu.is_error() {
                        return accu;
                    }
                }
            }
            _ => {
                return Value::error(EvalError::type_mismatch(
                    "list or map",
                    &range_val.cel_type().display_name(),
                ))
            }
        }

        // Compute final result
        let result_activation =
            HierarchicalActivation::new(self.activation).with_binding(accu_var, accu);
        let result_eval = self.child_evaluator(&result_activation);
        result_eval.eval_expr(result)
    }

    fn eval_member_test(&self, expr: &SpannedExpr, field: &str) -> Value {
        let value = self.eval_expr(expr);

        // For `has()`, we don't propagate errors - we just check presence
        if value.is_error() {
            return Value::Bool(false);
        }

        match &value {
            Value::Map(map) => {
                let key = MapKey::String(Arc::from(field));
                Value::Bool(map.contains_key(&key))
            }
            Value::Proto(proto) => {
                // Check if the proto message has the field set
                let descriptor = proto.descriptor();
                match descriptor.get_field_by_name(field) {
                    Some(field_desc) => {
                        if field_desc.supports_presence() {
                            // Proto2 fields, oneof fields, and message fields track presence
                            Value::Bool(proto.message().has_field(&field_desc))
                        } else {
                            // Proto3 scalar: has() = value differs from default
                            let current = proto.message().get_field(&field_desc);
                            let default = field_desc.default_value();
                            Value::Bool(current.as_ref() != &default)
                        }
                    }
                    None => {
                        // Try extension field lookup
                        if let Some(registry) = self.proto_types {
                            if let Some(ext) = registry.get_extension_by_name(field) {
                                if ext.containing_message() == descriptor {
                                    return Value::Bool(proto.message().has_extension(&ext));
                                }
                            }
                        }
                        Value::error(EvalError::field_not_found(field))
                    }
                }
            }
            _ => Value::Bool(false),
        }
    }

    fn eval_bind(&self, var_name: &str, init: &SpannedExpr, body: &SpannedExpr) -> Value {
        let init_val = self.eval_expr(init);
        if init_val.is_error() {
            return init_val;
        }

        let bind_activation =
            HierarchicalActivation::new(self.activation).with_binding(var_name, init_val);
        let bind_eval = self.child_evaluator(&bind_activation);
        bind_eval.eval_expr(body)
    }
}

/// Normalize nanoseconds to be in the range 0..999_999_999 for timestamps.
/// Returns (extra_seconds, normalized_nanos).
fn normalize_nanos(nanos: i64) -> (i64, i32) {
    if nanos >= 0 && nanos < 1_000_000_000 {
        (0, nanos as i32)
    } else if nanos >= 1_000_000_000 {
        let extra_secs = nanos / 1_000_000_000;
        let nanos = (nanos % 1_000_000_000) as i32;
        (extra_secs, nanos)
    } else {
        // Negative nanos - need to borrow from seconds
        // e.g., -999999998 nanos -> -1 second + 2 nanos
        let abs_nanos = (-nanos) as i64;
        let borrow_secs = (abs_nanos + 999_999_999) / 1_000_000_000;
        let remaining = (borrow_secs * 1_000_000_000 - abs_nanos) as i32;
        (-borrow_secs, remaining)
    }
}

/// Get the name of a timestamp component for error messages.
fn component_name(component: TimestampComponent) -> &'static str {
    match component {
        TimestampComponent::FullYear => "FullYear",
        TimestampComponent::Month => "Month",
        TimestampComponent::Date => "Date",
        TimestampComponent::DayOfMonth => "DayOfMonth",
        TimestampComponent::DayOfWeek => "DayOfWeek",
        TimestampComponent::DayOfYear => "DayOfYear",
        TimestampComponent::Hours => "Hours",
        TimestampComponent::Minutes => "Minutes",
        TimestampComponent::Seconds => "Seconds",
        TimestampComponent::Milliseconds => "Milliseconds",
    }
}

/// Format a double value according to CEL conventions.
fn format_double(d: f64) -> String {
    if d.is_nan() {
        "NaN".to_string()
    } else if d.is_infinite() {
        if d.is_sign_positive() {
            "+infinity".to_string()
        } else {
            "-infinity".to_string()
        }
    } else if d.fract() == 0.0 && d.abs() < 1e15 {
        format!("{:.1}", d)
    } else {
        d.to_string()
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    fn eval_expr(source: &str) -> Value {
        let result = parse(source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);
        let ast = result.ast.unwrap();

        let activation = super::super::EmptyActivation;
        let registry = FunctionRegistry::new();
        let evaluator = Evaluator::new(&activation, &registry);
        evaluator.eval(&ast)
    }

    fn eval_expr_with_vars(source: &str, vars: &[(&str, Value)]) -> Value {
        let result = parse(source);
        assert!(result.errors.is_empty(), "parse errors: {:?}", result.errors);
        let ast = result.ast.unwrap();

        let mut activation = super::super::MapActivation::new();
        for (name, value) in vars {
            activation.insert(*name, value.clone());
        }

        let registry = FunctionRegistry::new();
        let evaluator = Evaluator::new(&activation, &registry);
        evaluator.eval(&ast)
    }

    #[test]
    fn test_literals() {
        assert_eq!(eval_expr("null"), Value::Null);
        assert_eq!(eval_expr("true"), Value::Bool(true));
        assert_eq!(eval_expr("false"), Value::Bool(false));
        assert_eq!(eval_expr("42"), Value::Int(42));
        assert_eq!(eval_expr("42u"), Value::UInt(42));
        assert_eq!(eval_expr("3.14"), Value::Double(3.14));
        assert_eq!(eval_expr("\"hello\""), "hello".into());
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(eval_expr("1 + 2"), Value::Int(3));
        assert_eq!(eval_expr("5 - 3"), Value::Int(2));
        assert_eq!(eval_expr("3 * 4"), Value::Int(12));
        assert_eq!(eval_expr("10 / 3"), Value::Int(3));
        assert_eq!(eval_expr("10 % 3"), Value::Int(1));
    }

    #[test]
    fn test_comparison() {
        assert_eq!(eval_expr("1 < 2"), Value::Bool(true));
        assert_eq!(eval_expr("2 <= 2"), Value::Bool(true));
        assert_eq!(eval_expr("3 > 2"), Value::Bool(true));
        assert_eq!(eval_expr("2 >= 2"), Value::Bool(true));
        assert_eq!(eval_expr("1 == 1"), Value::Bool(true));
        assert_eq!(eval_expr("1 != 2"), Value::Bool(true));
    }

    #[test]
    fn test_logical() {
        assert_eq!(eval_expr("true && true"), Value::Bool(true));
        assert_eq!(eval_expr("true && false"), Value::Bool(false));
        assert_eq!(eval_expr("false || true"), Value::Bool(true));
        assert_eq!(eval_expr("false || false"), Value::Bool(false));
        assert_eq!(eval_expr("!true"), Value::Bool(false));
    }

    #[test]
    fn test_short_circuit() {
        // && short-circuits on false
        assert_eq!(eval_expr("false && undefined"), Value::Bool(false));
        // || short-circuits on true
        assert_eq!(eval_expr("true || undefined"), Value::Bool(true));
    }

    #[test]
    fn test_ternary() {
        assert_eq!(eval_expr("true ? 1 : 2"), Value::Int(1));
        assert_eq!(eval_expr("false ? 1 : 2"), Value::Int(2));
    }

    #[test]
    fn test_string_operations() {
        assert_eq!(eval_expr("\"hello\" + \" world\""), "hello world".into());
        assert_eq!(eval_expr("size(\"hello\")"), Value::Int(5));
        assert_eq!(
            eval_expr("\"hello\".contains(\"ell\")"),
            Value::Bool(true)
        );
        assert_eq!(
            eval_expr("\"hello\".startsWith(\"he\")"),
            Value::Bool(true)
        );
        assert_eq!(eval_expr("\"hello\".endsWith(\"lo\")"), Value::Bool(true));
    }

    #[test]
    fn test_list_operations() {
        assert_eq!(eval_expr("[1, 2, 3][0]"), Value::Int(1));
        assert_eq!(eval_expr("[1, 2, 3][2]"), Value::Int(3));
        assert_eq!(eval_expr("size([1, 2, 3])"), Value::Int(3));
        assert_eq!(eval_expr("2 in [1, 2, 3]"), Value::Bool(true));
        assert_eq!(eval_expr("4 in [1, 2, 3]"), Value::Bool(false));
    }

    #[test]
    fn test_map_operations() {
        assert_eq!(eval_expr("{\"a\": 1, \"b\": 2}[\"a\"]"), Value::Int(1));
        assert_eq!(eval_expr("{\"a\": 1, \"b\": 2}.a"), Value::Int(1));
        assert_eq!(eval_expr("size({\"a\": 1, \"b\": 2})"), Value::Int(2));
        assert_eq!(
            eval_expr("\"a\" in {\"a\": 1, \"b\": 2}"),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_variables() {
        assert_eq!(
            eval_expr_with_vars("x + 1", &[("x", Value::Int(41))]),
            Value::Int(42)
        );
        assert_eq!(
            eval_expr_with_vars("x && y", &[("x", Value::Bool(true)), ("y", Value::Bool(false))]),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_type_conversions() {
        assert_eq!(eval_expr("int(3.7)"), Value::Int(3));
        assert_eq!(eval_expr("double(42)"), Value::Double(42.0));
        assert_eq!(eval_expr("string(42)"), "42".into());
        assert_eq!(eval_expr("int(\"42\")"), Value::Int(42));
    }

    #[test]
    fn test_division_by_zero() {
        let result = eval_expr("1 / 0");
        assert!(result.is_error());
    }

    #[test]
    fn test_overflow() {
        let result = eval_expr("9223372036854775807 + 1");
        assert!(result.is_error());
    }
}
