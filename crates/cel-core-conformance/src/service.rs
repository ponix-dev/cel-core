//! CelConformanceService implementation.
//!
//! This module provides the concrete implementation of ConformanceService
//! using the cel-core unified Env.

use std::sync::Arc;

use prost::Message;
use prost_reflect::prost_types;
use prost_reflect::DynamicMessage;

use crate::{
    Binding, CheckResponse, ConformanceService, EvalResponse, Issue, ParseResponse, TypeDecl,
};
use cel_core::eval::wkt;
use cel_core::eval::{MapActivation, MapKey, Value, ValueMap};
use cel_core::types::ProtoTypeRegistry;
use cel_core::Env;
use cel_core_proto::gen::cel::expr::value::Kind as ProtoValueKind;
use cel_core_proto::gen::cel::expr::{
    expr_value, ErrorSet, ExprValue, ListValue, MapValue, ParsedExpr, Value as ProtoValue,
};
use cel_core_proto::{cel_type_from_proto, from_parsed_expr, to_checked_expr};

#[cfg(test)]
use cel_core::CelType;
#[cfg(test)]
use cel_core_proto::cel_type_to_proto;

/// CEL conformance service implementation using cel-core Env.
///
/// All operations go through the unified Env API:
/// - Parse: Uses Env::parse()
/// - Check: Uses Env::check()
/// - Eval: Stub (returns unimplemented error)
#[derive(Debug)]
pub struct CelConformanceService {
    /// Base environment with standard library.
    /// Extended per-request with type declarations.
    env: Env,
}

impl CelConformanceService {
    pub fn new() -> Self {
        // Create proto type registry with conformance test proto descriptors
        let mut registry = ProtoTypeRegistry::new();

        // Add conformance test proto descriptors
        // Order matters: dependencies must be added before dependents
        registry
            .add_file_descriptor_set(cel_core_proto::gen::cel::expr::FILE_DESCRIPTOR_SET)
            .expect("Failed to add cel.expr descriptors");
        registry
            .add_file_descriptor_set(cel_core_proto::gen::cel::expr::conformance::FILE_DESCRIPTOR_SET)
            .expect("Failed to add cel.expr.conformance descriptors");
        registry
            .add_file_descriptor_set(cel_core_proto::gen::cel::expr::conformance::proto2::FILE_DESCRIPTOR_SET)
            .expect("Failed to add cel.expr.conformance.proto2 descriptors");
        registry
            .add_file_descriptor_set(cel_core_proto::gen::cel::expr::conformance::proto3::FILE_DESCRIPTOR_SET)
            .expect("Failed to add cel.expr.conformance.proto3 descriptors");
        registry
            .add_file_descriptor_set(cel_core_proto::gen::cel::expr::conformance::test::FILE_DESCRIPTOR_SET)
            .expect("Failed to add cel.expr.conformance.test descriptors");

        Self {
            env: Env::with_standard_library()
                .with_all_extensions()
                .with_proto_types(registry),
        }
    }
}

impl Default for CelConformanceService {
    fn default() -> Self {
        Self::new()
    }
}

impl ConformanceService for CelConformanceService {
    fn parse(&self, source: &str) -> ParseResponse {
        // Use env's parser
        let result = self.env.parse(source);

        // Convert parse errors to issues
        let issues: Vec<Issue> = result
            .errors
            .iter()
            .map(|e| {
                // TODO: Convert byte offset to line/column if needed
                Issue::error(&e.message)
            })
            .collect();

        // Convert AST to ParsedExpr using cel-proto (with macro_calls for IDE features)
        // cel_core_proto now uses cel_core types directly
        let parsed_expr = result.ast.map(|ast| {
            cel_core_proto::to_parsed_expr_with_macros(&ast, source, &result.macro_calls)
        });

        ParseResponse { parsed_expr, issues }
    }

    fn check(&self, parsed: &ParsedExpr, type_env: &[TypeDecl], container: &str) -> CheckResponse {
        // Convert ParsedExpr back to AST
        // cel_core_proto::from_parsed_expr returns cel_core::SpannedExpr directly now
        let ast = match from_parsed_expr(parsed) {
            Ok(ast) => ast,
            Err(e) => {
                return CheckResponse {
                    checked_expr: None,
                    issues: vec![Issue::error(format!("Failed to convert ParsedExpr: {}", e))],
                };
            }
        };

        // Clone base env and add type declarations
        let mut env = self.env.clone();

        // Set the container for qualified name resolution
        env.set_container(container);

        for decl in type_env {
            // cel_type_from_proto now returns cel_core::CelType directly
            let cel_type = cel_type_from_proto(&decl.cel_type);
            env.add_variable(&decl.name, cel_type);
        }

        // Run the type checker using env
        let check_result = env.check(&ast);

        // Convert errors to issues
        let mut issues: Vec<Issue> = check_result
            .errors
            .iter()
            .map(|e| Issue::error(e.message()))
            .collect();

        // Build CheckedExpr using the helper from cel-core-proto
        // cel_core_proto::to_checked_expr now takes cel_core::CheckResult directly
        let checked_expr = if check_result.is_ok() || !check_result.type_map.is_empty() {
            Some(to_checked_expr(&check_result, parsed))
        } else {
            None
        };

        // If we have errors but also a partial result, still return the checked_expr
        if !issues.is_empty() && checked_expr.is_none() {
            issues.push(Issue::error("Type checking failed"));
        }

        CheckResponse {
            checked_expr,
            issues,
        }
    }

    fn eval(&self, expr: &ParsedExpr, bindings: &[Binding], type_env: &[TypeDecl], container: &str) -> EvalResponse {
        // Convert ParsedExpr to AST
        let parsed_ast = match from_parsed_expr(expr) {
            Ok(ast) => ast,
            Err(e) => {
                return EvalResponse {
                    result: None,
                    issues: vec![Issue::error(format!("Failed to convert ParsedExpr: {}", e))],
                };
            }
        };

        // Get proto registry for value conversion
        let proto_types = match self.env.proto_types() {
            Some(registry) => registry,
            None => {
                return EvalResponse {
                    result: None,
                    issues: vec![Issue::error("proto type registry not available")],
                };
            }
        };

        // Convert bindings to activation
        let activation = match bindings_to_activation(bindings, proto_types) {
            Ok(act) => act,
            Err(e) => {
                return EvalResponse {
                    result: None,
                    issues: vec![Issue::error(e)],
                };
            }
        };

        // Clone env and set container + type declarations for proper name resolution
        let mut env = self.env.clone();
        if !container.is_empty() {
            env.set_container(container);
        }
        for decl in type_env {
            let cel_type = cel_type_from_proto(&decl.cel_type);
            env.add_variable(&decl.name, cel_type);
        }

        // Run the checker to get type info (needed for proto message construction)
        // We use the check result even if there are errors, as we still get useful
        // reference_map entries for type resolution
        let check_result = env.check(&parsed_ast);
        let checked_ast = cel_core::Ast::new_checked(parsed_ast, "", check_result);

        match env.program(&checked_ast) {
            Ok(program) => {
                let result = program.eval_with_container(&activation, container);
                let expr_value = value_to_proto(&result);
                EvalResponse {
                    result: Some(expr_value),
                    issues: vec![],
                }
            }
            Err(e) => EvalResponse {
                result: None,
                issues: vec![Issue::error(format!("Failed to create program: {}", e))],
            },
        }
    }
}

/// Convert bindings to a MapActivation.
fn bindings_to_activation(
    bindings: &[Binding],
    registry: &ProtoTypeRegistry,
) -> Result<MapActivation, String> {
    let mut activation = MapActivation::new();
    for binding in bindings {
        let value = proto_value_to_value(&binding.value, registry)?;
        activation.insert(&binding.name, value);
    }
    Ok(activation)
}

/// Convert a proto Value to a cel_core::eval::Value.
fn proto_value_to_value(proto: &ProtoValue, registry: &ProtoTypeRegistry) -> Result<Value, String> {
    match &proto.kind {
        Some(ProtoValueKind::NullValue(_)) => Ok(Value::Null),
        Some(ProtoValueKind::BoolValue(b)) => Ok(Value::Bool(*b)),
        Some(ProtoValueKind::Int64Value(i)) => Ok(Value::Int(*i)),
        Some(ProtoValueKind::Uint64Value(u)) => Ok(Value::UInt(*u)),
        Some(ProtoValueKind::DoubleValue(d)) => Ok(Value::Double(*d)),
        Some(ProtoValueKind::StringValue(s)) => Ok(Value::String(Arc::from(s.as_str()))),
        Some(ProtoValueKind::BytesValue(b)) => Ok(Value::Bytes(Arc::from(b.as_ref()))),
        Some(ProtoValueKind::ListValue(list)) => {
            let values: Result<Vec<_>, _> = list
                .values
                .iter()
                .map(|v| proto_value_to_value(v, registry))
                .collect();
            Ok(Value::List(Arc::from(values?)))
        }
        Some(ProtoValueKind::MapValue(map)) => {
            let mut value_map = ValueMap::new();
            for entry in &map.entries {
                let key = entry.key.as_ref().ok_or("missing map key")?;
                let value = entry.value.as_ref().ok_or("missing map value")?;
                let key_value = proto_value_to_value(key, registry)?;
                let map_key = MapKey::from_value(&key_value)
                    .ok_or_else(|| format!("invalid map key type: {:?}", key_value))?;
                value_map.insert(map_key, proto_value_to_value(value, registry)?);
            }
            Ok(Value::Map(Arc::new(value_map)))
        }
        Some(ProtoValueKind::TypeValue(name)) => Ok(Value::new_type(name.as_str())),
        Some(ProtoValueKind::EnumValue(ev)) => {
            // Enum values are represented as ints in CEL
            Ok(Value::Int(ev.value as i64))
        }
        Some(ProtoValueKind::ObjectValue(any)) => {
            // Decode the Any message using the proto registry
            // Extract the type name from the type_url
            let type_name = any
                .type_url
                .strip_prefix("type.googleapis.com/")
                .unwrap_or(&any.type_url);

            // Get the message descriptor
            let descriptor = registry
                .get_message(type_name)
                .ok_or_else(|| format!("unknown message type: {}", type_name))?;

            // Decode the message
            let message = DynamicMessage::decode(descriptor, any.value.as_ref())
                .map_err(|e| format!("failed to decode message: {}", e))?;

            // Convert to CEL Value using well-known type unwrapping
            Ok(wkt::maybe_unwrap_well_known(message))
        }
        None => Err("missing value kind".to_string()),
    }
}

/// Convert a cel_core::eval::Value to a proto ExprValue.
fn value_to_proto(value: &Value) -> ExprValue {
    match value {
        Value::Error(e) => {
            // For debugging, include the error message
            use cel_core_proto::gen::cel::expr::Status;
            ExprValue {
                kind: Some(expr_value::Kind::Error(ErrorSet {
                    errors: vec![Status {
                        code: 0,
                        message: e.to_string(),
                        details: vec![],
                    }],
                })),
            }
        }
        _ => ExprValue {
            kind: Some(expr_value::Kind::Value(value_to_proto_value(value))),
        },
    }
}

/// Convert a cel_core::eval::Value to a proto Value.
fn value_to_proto_value(value: &Value) -> ProtoValue {
    let kind = match value {
        Value::Null => ProtoValueKind::NullValue(0),
        Value::Bool(b) => ProtoValueKind::BoolValue(*b),
        Value::Int(i) => ProtoValueKind::Int64Value(*i),
        Value::UInt(u) => ProtoValueKind::Uint64Value(*u),
        Value::Double(d) => ProtoValueKind::DoubleValue(*d),
        Value::String(s) => ProtoValueKind::StringValue(s.to_string()),
        Value::Bytes(b) => ProtoValueKind::BytesValue(b.to_vec().into()),
        Value::List(list) => ProtoValueKind::ListValue(ListValue {
            values: list.iter().map(value_to_proto_value).collect(),
        }),
        Value::Map(map) => ProtoValueKind::MapValue(MapValue {
            entries: map
                .iter()
                .map(|(k, v)| cel_core_proto::gen::cel::expr::map_value::Entry {
                    key: Some(value_to_proto_value(&k.to_value())),
                    value: Some(value_to_proto_value(v)),
                })
                .collect(),
        }),
        Value::Type(t) => ProtoValueKind::TypeValue(t.name.to_string()),
        Value::Timestamp(t) => {
            // Serialize as Any-wrapped google.protobuf.Timestamp
            let ts = prost_types::Timestamp {
                seconds: t.seconds,
                nanos: t.nanos,
            };
            let mut buf = Vec::new();
            ts.encode(&mut buf).unwrap();
            ProtoValueKind::ObjectValue(prost_types::Any {
                type_url: "type.googleapis.com/google.protobuf.Timestamp".to_string(),
                value: buf.into(),
            })
        }
        Value::Duration(d) => {
            // Serialize as Any-wrapped google.protobuf.Duration
            let dur = prost_types::Duration {
                seconds: d.seconds,
                nanos: d.nanos,
            };
            let mut buf = Vec::new();
            dur.encode(&mut buf).unwrap();
            ProtoValueKind::ObjectValue(prost_types::Any {
                type_url: "type.googleapis.com/google.protobuf.Duration".to_string(),
                value: buf.into(),
            })
        }
        Value::Optional(opt) => match opt {
            cel_core::eval::OptionalValue::None => ProtoValueKind::NullValue(0),
            cel_core::eval::OptionalValue::Some(v) => {
                return value_to_proto_value(v);
            }
        },
        Value::Proto(proto) => {
            // If the proto message IS an Any, preserve its inner type_url and value
            if proto.type_name() == "google.protobuf.Any" {
                let msg = proto.message();
                let desc = proto.descriptor();
                let type_url = desc
                    .get_field_by_name("type_url")
                    .map(|f| {
                        if let prost_reflect::Value::String(s) = msg.get_field(&f).into_owned() {
                            s
                        } else {
                            String::new()
                        }
                    })
                    .unwrap_or_default();
                let value_bytes = desc
                    .get_field_by_name("value")
                    .map(|f| {
                        if let prost_reflect::Value::Bytes(b) = msg.get_field(&f).into_owned() {
                            b.to_vec()
                        } else {
                            Vec::new()
                        }
                    })
                    .unwrap_or_default();
                ProtoValueKind::ObjectValue(prost_types::Any {
                    type_url,
                    value: value_bytes.into(),
                })
            } else {
                // Serialize proto message to Any format
                let type_url = format!("type.googleapis.com/{}", proto.type_name());
                let value_bytes = proto.message().encode_to_vec();
                ProtoValueKind::ObjectValue(prost_types::Any {
                    type_url,
                    value: value_bytes.into(),
                })
            }
        }
        Value::Error(_) => {
            // Errors are handled at the ExprValue level
            ProtoValueKind::NullValue(0)
        }
    };

    ProtoValue { kind: Some(kind) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_int() {
        let service = CelConformanceService::new();
        let result = service.parse("42");
        assert!(result.is_ok());
        assert!(result.parsed_expr.is_some());
    }

    #[test]
    fn test_parse_binary_op() {
        let service = CelConformanceService::new();
        let result = service.parse("1 + 2");
        assert!(result.is_ok());
        assert!(result.parsed_expr.is_some());
    }

    #[test]
    fn test_parse_with_error() {
        let service = CelConformanceService::new();
        let result = service.parse("1 +");
        assert!(!result.issues.is_empty());
    }

    #[test]
    fn test_check_literal() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("42").parsed_expr.unwrap();
        let check_result = service.check(&parse_result, &[], "");
        assert!(check_result.is_ok());
        assert!(check_result.checked_expr.is_some());

        let checked = check_result.checked_expr.unwrap();
        assert!(!checked.type_map.is_empty());
    }

    #[test]
    fn test_check_with_variable() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x + 1").parsed_expr.unwrap();

        // Provide type declaration for x
        let type_decl = TypeDecl {
            name: "x".to_string(),
            cel_type: cel_type_to_proto(&CelType::Int),
        };

        let check_result = service.check(&parse_result, &[type_decl], "");
        assert!(check_result.is_ok());
        assert!(check_result.checked_expr.is_some());
    }

    #[test]
    fn test_check_undefined_variable() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x").parsed_expr.unwrap();
        let check_result = service.check(&parse_result, &[], "");
        assert!(!check_result.is_ok());
        assert!(check_result
            .issues
            .iter()
            .any(|i| i.message.contains("undeclared")));
    }

    #[test]
    fn test_check_type_mismatch() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("x + \"str\"").parsed_expr.unwrap();

        let type_decl = TypeDecl {
            name: "x".to_string(),
            cel_type: cel_type_to_proto(&CelType::Int),
        };

        let check_result = service.check(&parse_result, &[type_decl], "");
        assert!(!check_result.is_ok());
        assert!(check_result
            .issues
            .iter()
            .any(|i| i.message.contains("no matching overload")));
    }

    #[test]
    fn test_eval_literal() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("42").parsed_expr.unwrap();
        let eval_result = service.eval(&parse_result, &[], &[], "");
        assert!(eval_result.is_ok(), "eval should succeed: {:?}", eval_result.issues);
        assert!(eval_result.result.is_some());

        let result = eval_result.result.unwrap();
        match &result.kind {
            Some(expr_value::Kind::Value(v)) => {
                match &v.kind {
                    Some(ProtoValueKind::Int64Value(42)) => {}
                    other => panic!("expected Int64Value(42), got {:?}", other),
                }
            }
            other => panic!("expected Value, got {:?}", other),
        }
    }

    #[test]
    fn test_eval_with_variable() {
        use cel_core_proto::gen::cel::expr::value::Kind as ValueKind;

        let service = CelConformanceService::new();
        let parse_result = service.parse("x + 1").parsed_expr.unwrap();

        // Create a binding for x = 41
        let binding = Binding {
            name: "x".to_string(),
            value: cel_core_proto::gen::cel::expr::Value {
                kind: Some(ValueKind::Int64Value(41)),
            },
        };

        let eval_result = service.eval(&parse_result, &[binding], &[], "");
        assert!(eval_result.is_ok(), "eval should succeed: {:?}", eval_result.issues);

        let result = eval_result.result.unwrap();
        match &result.kind {
            Some(expr_value::Kind::Value(v)) => {
                match &v.kind {
                    Some(ProtoValueKind::Int64Value(42)) => {}
                    other => panic!("expected Int64Value(42), got {:?}", other),
                }
            }
            other => panic!("expected Value, got {:?}", other),
        }
    }

    #[test]
    fn test_eval_unknown_variable() {
        let service = CelConformanceService::new();
        let parse_result = service.parse("unknown_var").parsed_expr.unwrap();
        let eval_result = service.eval(&parse_result, &[], &[], "");

        // Should return an error result
        let result = eval_result.result.unwrap();
        match &result.kind {
            Some(expr_value::Kind::Error(_)) => {}
            other => panic!("expected Error, got {:?}", other),
        }
    }
}
