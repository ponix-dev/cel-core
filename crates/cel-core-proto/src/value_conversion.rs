//! Conversion between proto `cel.expr.Value` / `cel.expr.ExprValue` and cel-core `Value`.
//!
//! These functions enable proto↔CEL value interop for gRPC integration,
//! serialized evaluation contexts, and any scenario where CEL values need
//! to cross a protobuf wire boundary.

use std::sync::Arc;

use prost::Message;
use prost_reflect::DynamicMessage;

use cel_core::{MapKey, Value, ValueMap};

use crate::gen::cel::expr::decl::function_decl::Overload;
use crate::gen::cel::expr::value::Kind as ProtoValueKind;
use crate::gen::cel::expr::{
    expr_value, ErrorSet, ExprValue, ListValue, MapValue, Status, Value as ProtoValue,
};
use crate::message::ProstMessage;
use crate::registry::ProstProtoRegistry;
use crate::type_conversion::cel_type_from_proto;
use crate::wkt::maybe_unwrap_well_known;

/// Convert a proto `cel.expr.Value` to a cel-core [`Value`].
///
/// Handles all value kinds: null, bool, int, uint, double, string, bytes,
/// list, map, type, enum, and object (Any).
///
/// # Errors
///
/// Returns an error string if the value kind is missing or an object value
/// cannot be decoded.
pub fn proto_value_to_value(
    proto: &ProtoValue,
    registry: &ProstProtoRegistry,
) -> Result<Value, String> {
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
            let type_name = any
                .type_url
                .strip_prefix("type.googleapis.com/")
                .unwrap_or(&any.type_url);

            let descriptor = registry
                .get_message(type_name)
                .ok_or_else(|| format!("unknown message type: {}", type_name))?;

            let message = DynamicMessage::decode(descriptor, any.value.as_ref())
                .map_err(|e| format!("failed to decode message: {}", e))?;

            Ok(maybe_unwrap_well_known(message))
        }
        None => Err("missing value kind".to_string()),
    }
}

/// Convert a cel-core [`Value`] to a proto `cel.expr.Value`.
///
/// Handles all Value variants including Timestamp, Duration, Optional,
/// Message, Enum, and Error.
pub fn value_to_proto_value(value: &Value) -> ProtoValue {
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
                .map(|(k, v)| crate::gen::cel::expr::map_value::Entry {
                    key: Some(value_to_proto_value(&k.to_value())),
                    value: Some(value_to_proto_value(v)),
                })
                .collect(),
        }),
        Value::Type(t) => ProtoValueKind::TypeValue(t.name.to_string()),
        Value::Timestamp(t) => {
            let ts = prost_types::Timestamp {
                seconds: t.seconds,
                nanos: t.nanos,
            };
            let mut buf = Vec::new();
            ts.encode(&mut buf).unwrap();
            ProtoValueKind::ObjectValue(prost_types::Any {
                type_url: "type.googleapis.com/google.protobuf.Timestamp".to_string(),
                value: buf,
            })
        }
        Value::Duration(d) => {
            let dur = prost_types::Duration {
                seconds: d.seconds,
                nanos: d.nanos,
            };
            let mut buf = Vec::new();
            dur.encode(&mut buf).unwrap();
            ProtoValueKind::ObjectValue(prost_types::Any {
                type_url: "type.googleapis.com/google.protobuf.Duration".to_string(),
                value: buf,
            })
        }
        Value::Optional(opt) => match opt {
            cel_core::OptionalValue::None => ProtoValueKind::NullValue(0),
            cel_core::OptionalValue::Some(v) => {
                return value_to_proto_value(v);
            }
        },
        Value::Message(msg_val) => {
            let proto = msg_val
                .as_any()
                .downcast_ref::<ProstMessage>()
                .expect("Message must be ProstMessage");
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
                    value: value_bytes,
                })
            } else {
                let type_url = format!("type.googleapis.com/{}", proto.type_name());
                let value_bytes = proto.message().encode_to_vec();
                ProtoValueKind::ObjectValue(prost_types::Any {
                    type_url,
                    value: value_bytes,
                })
            }
        }
        Value::Enum(e) => ProtoValueKind::EnumValue(crate::gen::cel::expr::EnumValue {
            r#type: e.type_name.to_string(),
            value: e.value,
        }),
        Value::Error(_) => {
            // Errors are handled at the ExprValue level
            ProtoValueKind::NullValue(0)
        }
    };

    ProtoValue { kind: Some(kind) }
}

/// Convert a cel-core [`Value`] to a proto `cel.expr.ExprValue`.
///
/// This wraps [`value_to_proto_value`] and additionally handles `Value::Error`
/// by producing an `ExprValue` with an `ErrorSet` instead of a plain value.
pub fn value_to_expr_value(value: &Value) -> ExprValue {
    match value {
        Value::Error(e) => ExprValue {
            kind: Some(expr_value::Kind::Error(ErrorSet {
                errors: vec![Status {
                    code: 0,
                    message: e.to_string(),
                    details: vec![],
                }],
            })),
        },
        _ => ExprValue {
            kind: Some(expr_value::Kind::Value(value_to_proto_value(value))),
        },
    }
}

/// Convert a proto function declaration to a cel-core [`FunctionDecl`](cel_core::FunctionDecl).
///
/// Accepts the function name and its proto overloads directly, so callers
/// can pass fields from any wrapper struct (e.g. a conformance `FunctionTypeDecl`
/// or a raw `cel.expr.Decl.FunctionDecl`).
pub fn function_decl_from_proto(name: &str, overloads: &[Overload]) -> cel_core::FunctionDecl {
    let mut func = cel_core::FunctionDecl::new(name);
    for overload in overloads {
        let params: Vec<_> = overload.params.iter().map(cel_type_from_proto).collect();
        let result = overload
            .result_type
            .as_ref()
            .map(cel_type_from_proto)
            .unwrap_or(cel_core::CelType::Dyn);
        let mut ovl = if overload.is_instance_function {
            cel_core::OverloadDecl::method(&overload.overload_id, params, result)
        } else {
            cel_core::OverloadDecl::function(&overload.overload_id, params, result)
        };
        if !overload.type_params.is_empty() {
            ovl = ovl.with_type_params(overload.type_params.clone());
        }
        func = func.with_overload(ovl);
    }
    func
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen::cel::expr::value::Kind as ValueKind;
    use crate::gen::cel::expr::Value as ProtoValue;

    fn new_registry() -> ProstProtoRegistry {
        let mut registry = ProstProtoRegistry::new();
        registry
            .add_file_descriptor_set(crate::gen::cel::expr::FILE_DESCRIPTOR_SET)
            .expect("Failed to add cel.expr descriptors");
        registry
    }

    // ==================== proto_value_to_value tests ====================

    #[test]
    fn test_null_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::NullValue(0)),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        assert!(matches!(value, Value::Null));
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::NullValue(0))));
    }

    #[test]
    fn test_bool_roundtrip() {
        let registry = new_registry();
        for b in [true, false] {
            let proto = ProtoValue {
                kind: Some(ValueKind::BoolValue(b)),
            };
            let value = proto_value_to_value(&proto, &registry).unwrap();
            assert_eq!(value, Value::Bool(b));
            let back = value_to_proto_value(&value);
            assert!(matches!(back.kind, Some(ValueKind::BoolValue(v)) if v == b));
        }
    }

    #[test]
    fn test_int_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::Int64Value(42)),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        assert_eq!(value, Value::Int(42));
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::Int64Value(42))));
    }

    #[test]
    fn test_uint_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::Uint64Value(99)),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        assert_eq!(value, Value::UInt(99));
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::Uint64Value(99))));
    }

    #[test]
    fn test_double_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::DoubleValue(1.5)),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        assert_eq!(value, Value::Double(1.5));
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::DoubleValue(d)) if d == 1.5));
    }

    #[test]
    fn test_string_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::StringValue("hello".to_string())),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        assert_eq!(value, Value::String(Arc::from("hello")));
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::StringValue(ref s)) if s == "hello"));
    }

    #[test]
    fn test_bytes_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::BytesValue(vec![1, 2, 3].into())),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        assert_eq!(value, Value::Bytes(Arc::from(vec![1u8, 2, 3].as_slice())));
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::BytesValue(ref b)) if b.as_ref() == [1, 2, 3]));
    }

    #[test]
    fn test_list_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::ListValue(ListValue {
                values: vec![
                    ProtoValue {
                        kind: Some(ValueKind::Int64Value(1)),
                    },
                    ProtoValue {
                        kind: Some(ValueKind::Int64Value(2)),
                    },
                ],
            })),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        match &value {
            Value::List(l) => {
                assert_eq!(l.len(), 2);
                assert_eq!(l[0], Value::Int(1));
                assert_eq!(l[1], Value::Int(2));
            }
            _ => panic!("expected list"),
        }
        let back = value_to_proto_value(&value);
        match back.kind {
            Some(ValueKind::ListValue(ref l)) => assert_eq!(l.values.len(), 2),
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn test_map_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::MapValue(MapValue {
                entries: vec![crate::gen::cel::expr::map_value::Entry {
                    key: Some(ProtoValue {
                        kind: Some(ValueKind::StringValue("key".to_string())),
                    }),
                    value: Some(ProtoValue {
                        kind: Some(ValueKind::Int64Value(10)),
                    }),
                }],
            })),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        match &value {
            Value::Map(m) => {
                assert_eq!(m.len(), 1);
                let key = MapKey::String(Arc::from("key"));
                assert_eq!(m.get(&key), Some(&Value::Int(10)));
            }
            _ => panic!("expected map"),
        }
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::MapValue(_))));
    }

    #[test]
    fn test_type_roundtrip() {
        let registry = new_registry();
        let proto = ProtoValue {
            kind: Some(ValueKind::TypeValue("int".to_string())),
        };
        let value = proto_value_to_value(&proto, &registry).unwrap();
        match &value {
            Value::Type(t) => assert_eq!(t.name.as_ref(), "int"),
            _ => panic!("expected type value"),
        }
        let back = value_to_proto_value(&value);
        assert!(matches!(back.kind, Some(ValueKind::TypeValue(ref s)) if s == "int"));
    }

    #[test]
    fn test_enum_to_proto() {
        let value = Value::Enum(cel_core::EnumValue::new("my.Enum", 2));
        let back = value_to_proto_value(&value);
        match back.kind {
            Some(ValueKind::EnumValue(ref ev)) => {
                assert_eq!(ev.r#type, "my.Enum");
                assert_eq!(ev.value, 2);
            }
            _ => panic!("expected enum value"),
        }
    }

    // ==================== value_to_expr_value tests ====================

    #[test]
    fn test_expr_value_normal() {
        let value = Value::Int(42);
        let expr = value_to_expr_value(&value);
        match expr.kind {
            Some(expr_value::Kind::Value(ref v)) => {
                assert!(matches!(v.kind, Some(ValueKind::Int64Value(42))));
            }
            _ => panic!("expected value"),
        }
    }

    #[test]
    fn test_expr_value_error() {
        let value = Value::error(cel_core::EvalError::internal("test error"));
        let expr = value_to_expr_value(&value);
        match expr.kind {
            Some(expr_value::Kind::Error(ref es)) => {
                assert_eq!(es.errors.len(), 1);
                assert!(es.errors[0].message.contains("test error"));
            }
            _ => panic!("expected error"),
        }
    }

    // ==================== function_decl_from_proto tests ====================

    #[test]
    fn test_function_decl_standalone() {
        use crate::type_conversion::cel_type_to_proto;

        let overloads = vec![Overload {
            overload_id: "size_list".to_string(),
            params: vec![cel_type_to_proto(&cel_core::CelType::list(
                cel_core::CelType::Dyn,
            ))],
            type_params: vec![],
            result_type: Some(cel_type_to_proto(&cel_core::CelType::Int)),
            is_instance_function: false,
            doc: String::new(),
        }];

        let decl = function_decl_from_proto("size", &overloads);
        assert_eq!(decl.name, "size");
        assert_eq!(decl.overloads.len(), 1);
        assert!(!decl.overloads[0].is_member);
    }

    #[test]
    fn test_function_decl_instance() {
        use crate::type_conversion::cel_type_to_proto;

        let overloads = vec![Overload {
            overload_id: "string_contains".to_string(),
            params: vec![
                cel_type_to_proto(&cel_core::CelType::String),
                cel_type_to_proto(&cel_core::CelType::String),
            ],
            type_params: vec![],
            result_type: Some(cel_type_to_proto(&cel_core::CelType::Bool)),
            is_instance_function: true,
            doc: String::new(),
        }];

        let decl = function_decl_from_proto("contains", &overloads);
        assert_eq!(decl.name, "contains");
        assert_eq!(decl.overloads.len(), 1);
        assert!(decl.overloads[0].is_member);
    }

    #[test]
    fn test_missing_value_kind() {
        let registry = new_registry();
        let proto = ProtoValue { kind: None };
        let result = proto_value_to_value(&proto, &registry);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "missing value kind");
    }
}
