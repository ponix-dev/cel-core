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

use std::sync::Arc;

use super::{
    time::{self, TimestampComponent},
    Activation, EvalError, FunctionRegistry, HierarchicalActivation, MapKey, OptionalValue,
    TypeValue, Value, ValueMap,
};
use crate::types::{BinaryOp, Expr, SpannedExpr, UnaryOp};

/// The CEL expression evaluator.
///
/// Evaluates a CEL AST against an activation (variable bindings) and
/// function registry.
pub struct Evaluator<'a> {
    activation: &'a dyn Activation,
    functions: &'a FunctionRegistry,
}

impl<'a> Evaluator<'a> {
    /// Create a new evaluator.
    pub fn new(activation: &'a dyn Activation, functions: &'a FunctionRegistry) -> Self {
        Self {
            activation,
            functions,
        }
    }

    /// Evaluate an expression.
    pub fn eval(&self, expr: &SpannedExpr) -> Value {
        self.eval_expr(expr)
    }

    fn eval_expr(&self, expr: &SpannedExpr) -> Value {
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
            _ => {
                return Value::error(EvalError::type_mismatch(
                    "bool",
                    &left_val.cel_type().display_name(),
                ))
            }
        }

        // Left is true, evaluate right
        let right_val = self.eval_expr(right);
        match &right_val {
            Value::Bool(_) | Value::Error(_) => right_val,
            _ => Value::error(EvalError::type_mismatch(
                "bool",
                &right_val.cel_type().display_name(),
            )),
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
            _ => {
                return Value::error(EvalError::type_mismatch(
                    "bool",
                    &left_val.cel_type().display_name(),
                ))
            }
        }

        // Left is false, evaluate right
        let right_val = self.eval_expr(right);
        match &right_val {
            Value::Bool(_) | Value::Error(_) => right_val,
            _ => Value::error(EvalError::type_mismatch(
                "bool",
                &right_val.cel_type().display_name(),
            )),
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
                if let Some(key) = MapKey::from_value(&left) {
                    Value::Bool(map.contains_key(&key))
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
            _ => {
                if optional {
                    Value::optional_none()
                } else {
                    Value::error(EvalError::field_not_found(field))
                }
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
                if let Some(key) = MapKey::from_value(index) {
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
        _type_name: &SpannedExpr,
        _fields: &[crate::types::StructField],
    ) -> Value {
        // Struct construction requires proto message support
        // For now, return an error
        Value::error(EvalError::internal(
            "struct construction not yet implemented",
        ))
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

                    let iter_eval = Evaluator::new(&iter_activation, self.functions);

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

                    let iter_eval = Evaluator::new(&iter_activation, self.functions);

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
        let result_eval = Evaluator::new(&result_activation, self.functions);
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
        let bind_eval = Evaluator::new(&bind_activation, self.functions);
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
