//! Conversion between CEL native types and proto types.
//!
//! This module provides bidirectional conversion between:
//! - `cel_core::CelType` and proto `Type`
//! - `cel_core::CelValue` and proto `Constant`
//!
//! ## Proto Round-Trip Limitations
//!
//! ### CelType
//! The cel-spec proto format does not distinguish between enum and message types.
//! Both `CelType::Enum` and `CelType::Message` serialize to `ProtoTypeKind::MessageType`.
//! When deserializing, all message types become `CelType::Message`.
//!
//! This means: `CelType::Enum("MyEnum")` -> proto -> `CelType::Message("MyEnum")`
//!
//! ### CelValue
//! The proto `Constant` also has deprecated Duration and Timestamp variants which
//! we don't support in `CelValue`. Converting from proto will return `None` for these.

use crate::gen::cel::expr::constant::ConstantKind;
use crate::gen::cel::expr::r#type::{
    AbstractType as ProtoAbstractType, FunctionType as ProtoFunctionType,
    ListType as ProtoListType, MapType as ProtoMapType, PrimitiveType as ProtoPrimitiveType,
    TypeKind as ProtoTypeKind, WellKnownType as ProtoWellKnownType,
};
use crate::gen::cel::expr::{Constant as ProtoConstant, Type as ProtoType};
use cel_core::types::{CelType, CelValue};
use std::sync::Arc;

/// Convert a proto Type to a CelType.
pub fn cel_type_from_proto(proto: &ProtoType) -> CelType {
    match &proto.type_kind {
        Some(ProtoTypeKind::Primitive(p)) => match ProtoPrimitiveType::try_from(*p) {
            Ok(ProtoPrimitiveType::Bool) => CelType::Bool,
            Ok(ProtoPrimitiveType::Int64) => CelType::Int,
            Ok(ProtoPrimitiveType::Uint64) => CelType::UInt,
            Ok(ProtoPrimitiveType::Double) => CelType::Double,
            Ok(ProtoPrimitiveType::String) => CelType::String,
            Ok(ProtoPrimitiveType::Bytes) => CelType::Bytes,
            _ => CelType::Dyn,
        },
        Some(ProtoTypeKind::WellKnown(w)) => match ProtoWellKnownType::try_from(*w) {
            Ok(ProtoWellKnownType::Timestamp) => CelType::Timestamp,
            Ok(ProtoWellKnownType::Duration) => CelType::Duration,
            _ => CelType::Dyn,
        },
        Some(ProtoTypeKind::ListType(list)) => {
            let elem = list
                .elem_type
                .as_ref()
                .map(|t| cel_type_from_proto(t))
                .unwrap_or(CelType::Dyn);
            CelType::List(Arc::new(elem))
        }
        Some(ProtoTypeKind::MapType(map)) => {
            let key = map
                .key_type
                .as_ref()
                .map(|t| cel_type_from_proto(t))
                .unwrap_or(CelType::Dyn);
            let val = map
                .value_type
                .as_ref()
                .map(|t| cel_type_from_proto(t))
                .unwrap_or(CelType::Dyn);
            CelType::Map(Arc::new(key), Arc::new(val))
        }
        Some(ProtoTypeKind::MessageType(name)) => {
            // Handle well-known types that should map to CEL native types
            match name.as_str() {
                "google.protobuf.Timestamp" => CelType::Timestamp,
                "google.protobuf.Duration" => CelType::Duration,
                "google.protobuf.BoolValue" => CelType::wrapper(CelType::Bool),
                "google.protobuf.Int32Value" | "google.protobuf.Int64Value" => {
                    CelType::wrapper(CelType::Int)
                }
                "google.protobuf.UInt32Value" | "google.protobuf.UInt64Value" => {
                    CelType::wrapper(CelType::UInt)
                }
                "google.protobuf.FloatValue" | "google.protobuf.DoubleValue" => {
                    CelType::wrapper(CelType::Double)
                }
                "google.protobuf.StringValue" => CelType::wrapper(CelType::String),
                "google.protobuf.BytesValue" => CelType::wrapper(CelType::Bytes),
                "google.protobuf.Any" | "google.protobuf.Struct" | "google.protobuf.Value"
                | "google.protobuf.ListValue" => CelType::Dyn,
                _ => CelType::Message(Arc::from(name.as_str())),
            }
        }
        Some(ProtoTypeKind::TypeParam(name)) => CelType::TypeParam(Arc::from(name.as_str())),
        Some(ProtoTypeKind::Type(inner)) => {
            let inner_type = cel_type_from_proto(inner.as_ref());
            CelType::Type(Arc::new(inner_type))
        }
        Some(ProtoTypeKind::Function(func)) => {
            let params: Vec<_> = func.arg_types.iter().map(cel_type_from_proto).collect();
            let result = func
                .result_type
                .as_ref()
                .map(|t| cel_type_from_proto(t))
                .unwrap_or(CelType::Dyn);
            CelType::Function {
                params: Arc::from(params),
                result: Arc::new(result),
            }
        }
        Some(ProtoTypeKind::Wrapper(w)) => {
            let inner = match ProtoPrimitiveType::try_from(*w) {
                Ok(ProtoPrimitiveType::Bool) => CelType::Bool,
                Ok(ProtoPrimitiveType::Int64) => CelType::Int,
                Ok(ProtoPrimitiveType::Uint64) => CelType::UInt,
                Ok(ProtoPrimitiveType::Double) => CelType::Double,
                Ok(ProtoPrimitiveType::String) => CelType::String,
                Ok(ProtoPrimitiveType::Bytes) => CelType::Bytes,
                _ => CelType::Dyn,
            };
            CelType::Wrapper(Arc::new(inner))
        }
        Some(ProtoTypeKind::Null(_)) => CelType::Null,
        Some(ProtoTypeKind::Dyn(_)) => CelType::Dyn,
        Some(ProtoTypeKind::Error(_)) => CelType::Error,
        Some(ProtoTypeKind::AbstractType(abs)) => {
            // Special case: "optional" abstract type maps to CelType::Optional
            if abs.name == "optional" && abs.parameter_types.len() == 1 {
                let inner = cel_type_from_proto(&abs.parameter_types[0]);
                return CelType::Optional(Arc::new(inner));
            }
            let params: Vec<_> = abs.parameter_types.iter().map(cel_type_from_proto).collect();
            CelType::Abstract {
                name: Arc::from(abs.name.as_str()),
                params: Arc::from(params),
            }
        }
        None => CelType::Dyn,
    }
}

/// Convert a CelType to a proto Type.
pub fn cel_type_to_proto(cel_type: &CelType) -> ProtoType {
    let type_kind = match cel_type {
        CelType::Bool => ProtoTypeKind::Primitive(ProtoPrimitiveType::Bool as i32),
        CelType::Int => ProtoTypeKind::Primitive(ProtoPrimitiveType::Int64 as i32),
        CelType::UInt => ProtoTypeKind::Primitive(ProtoPrimitiveType::Uint64 as i32),
        CelType::Double => ProtoTypeKind::Primitive(ProtoPrimitiveType::Double as i32),
        CelType::String => ProtoTypeKind::Primitive(ProtoPrimitiveType::String as i32),
        CelType::Bytes => ProtoTypeKind::Primitive(ProtoPrimitiveType::Bytes as i32),
        CelType::Timestamp => ProtoTypeKind::WellKnown(ProtoWellKnownType::Timestamp as i32),
        CelType::Duration => ProtoTypeKind::WellKnown(ProtoWellKnownType::Duration as i32),
        CelType::List(elem) => ProtoTypeKind::ListType(Box::new(ProtoListType {
            elem_type: Some(Box::new(cel_type_to_proto(elem))),
        })),
        CelType::Map(key, val) => ProtoTypeKind::MapType(Box::new(ProtoMapType {
            key_type: Some(Box::new(cel_type_to_proto(key))),
            value_type: Some(Box::new(cel_type_to_proto(val))),
        })),
        CelType::Null => ProtoTypeKind::Null(0),
        CelType::Dyn => ProtoTypeKind::Dyn(()),
        CelType::Type(inner) => ProtoTypeKind::Type(Box::new(cel_type_to_proto(inner))),
        CelType::Message(name) => ProtoTypeKind::MessageType(name.to_string()),
        // Note: Enum loses its distinction when serialized - see module docs
        CelType::Enum(name) => ProtoTypeKind::MessageType(name.to_string()),
        CelType::Abstract { name, params } => {
            ProtoTypeKind::AbstractType(ProtoAbstractType {
                name: name.to_string(),
                parameter_types: params.iter().map(cel_type_to_proto).collect(),
            })
        }
        CelType::Function { params, result } => {
            ProtoTypeKind::Function(Box::new(ProtoFunctionType {
                arg_types: params.iter().map(cel_type_to_proto).collect(),
                result_type: Some(Box::new(cel_type_to_proto(result))),
            }))
        }
        CelType::TypeParam(name) => ProtoTypeKind::TypeParam(name.to_string()),
        // TypeVar is internal for inference; serialize as Dyn
        CelType::TypeVar(_) => ProtoTypeKind::Dyn(()),
        CelType::Wrapper(inner) => {
            let prim = match inner.as_ref() {
                CelType::Bool => ProtoPrimitiveType::Bool as i32,
                CelType::Int => ProtoPrimitiveType::Int64 as i32,
                CelType::UInt => ProtoPrimitiveType::Uint64 as i32,
                CelType::Double => ProtoPrimitiveType::Double as i32,
                CelType::String => ProtoPrimitiveType::String as i32,
                CelType::Bytes => ProtoPrimitiveType::Bytes as i32,
                _ => ProtoPrimitiveType::Unspecified as i32,
            };
            ProtoTypeKind::Wrapper(prim)
        }
        // Optional is represented as an abstract type "optional" with a type parameter
        CelType::Optional(inner) => ProtoTypeKind::AbstractType(ProtoAbstractType {
            name: "optional".to_string(),
            parameter_types: vec![cel_type_to_proto(inner)],
        }),
        CelType::Error => ProtoTypeKind::Error(()),
    };

    ProtoType {
        type_kind: Some(type_kind),
    }
}

impl From<&ProtoType> for CelType {
    fn from(proto: &ProtoType) -> Self {
        cel_type_from_proto(proto)
    }
}

impl From<&CelType> for ProtoType {
    fn from(cel_type: &CelType) -> Self {
        cel_type_to_proto(cel_type)
    }
}

// ==================== CelValue <-> Constant ====================

/// Convert a proto Constant to a CelValue.
///
/// Returns `None` for deprecated Duration and Timestamp constant kinds.
pub fn cel_value_from_proto(proto: &ProtoConstant) -> Option<CelValue> {
    match &proto.constant_kind {
        Some(ConstantKind::NullValue(_)) => Some(CelValue::Null),
        Some(ConstantKind::BoolValue(v)) => Some(CelValue::Bool(*v)),
        Some(ConstantKind::Int64Value(v)) => Some(CelValue::Int(*v)),
        Some(ConstantKind::Uint64Value(v)) => Some(CelValue::UInt(*v)),
        Some(ConstantKind::DoubleValue(v)) => Some(CelValue::Double(*v)),
        Some(ConstantKind::StringValue(v)) => Some(CelValue::String(v.clone())),
        Some(ConstantKind::BytesValue(v)) => Some(CelValue::Bytes(v.to_vec())),
        // Deprecated constant kinds - not supported in CelValue
        Some(ConstantKind::DurationValue(_)) | Some(ConstantKind::TimestampValue(_)) => None,
        None => None,
    }
}

/// Convert a CelValue to a proto Constant.
pub fn cel_value_to_proto(value: &CelValue) -> ProtoConstant {
    let constant_kind = match value {
        CelValue::Null => ConstantKind::NullValue(0),
        CelValue::Bool(v) => ConstantKind::BoolValue(*v),
        CelValue::Int(v) => ConstantKind::Int64Value(*v),
        CelValue::UInt(v) => ConstantKind::Uint64Value(*v),
        CelValue::Double(v) => ConstantKind::DoubleValue(*v),
        CelValue::String(v) => ConstantKind::StringValue(v.clone()),
        CelValue::Bytes(v) => ConstantKind::BytesValue(v.clone().into()),
    };

    ProtoConstant {
        constant_kind: Some(constant_kind),
    }
}

impl From<&CelValue> for ProtoConstant {
    fn from(value: &CelValue) -> Self {
        cel_value_to_proto(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_roundtrip() {
        let types = [
            CelType::Bool,
            CelType::Int,
            CelType::UInt,
            CelType::Double,
            CelType::String,
            CelType::Bytes,
        ];

        for cel_type in types {
            let proto = cel_type_to_proto(&cel_type);
            let roundtripped = cel_type_from_proto(&proto);
            assert_eq!(cel_type, roundtripped);
        }
    }

    #[test]
    fn test_wellknown_roundtrip() {
        let types = [CelType::Timestamp, CelType::Duration];

        for cel_type in types {
            let proto = cel_type_to_proto(&cel_type);
            let roundtripped = cel_type_from_proto(&proto);
            assert_eq!(cel_type, roundtripped);
        }
    }

    #[test]
    fn test_list_roundtrip() {
        let cel_type = CelType::list(CelType::Int);
        let proto = cel_type_to_proto(&cel_type);
        let roundtripped = cel_type_from_proto(&proto);
        assert_eq!(cel_type, roundtripped);
    }

    #[test]
    fn test_map_roundtrip() {
        let cel_type = CelType::map(CelType::String, CelType::Int);
        let proto = cel_type_to_proto(&cel_type);
        let roundtripped = cel_type_from_proto(&proto);
        assert_eq!(cel_type, roundtripped);
    }

    #[test]
    fn test_nested_roundtrip() {
        let cel_type = CelType::map(CelType::String, CelType::list(CelType::Int));
        let proto = cel_type_to_proto(&cel_type);
        let roundtripped = cel_type_from_proto(&proto);
        assert_eq!(cel_type, roundtripped);
    }

    #[test]
    fn test_from_trait() {
        let cel_type = CelType::Int;
        let proto: ProtoType = (&cel_type).into();
        let back: CelType = (&proto).into();
        assert_eq!(cel_type, back);
    }

    #[test]
    fn test_abstract_type_roundtrip() {
        let cel_type = CelType::abstract_type("custom.Container", &[CelType::Int, CelType::String]);
        let proto = cel_type_to_proto(&cel_type);
        let roundtripped = cel_type_from_proto(&proto);
        assert_eq!(cel_type, roundtripped);
    }

    #[test]
    fn enum_roundtrip_is_lossy_by_design() {
        let cel_enum = CelType::enum_type("my.package.MyEnum");
        let proto = cel_type_to_proto(&cel_enum);
        let roundtripped = cel_type_from_proto(&proto);

        // Roundtrip produces Message, not Enum - this is expected
        assert_eq!(roundtripped, CelType::message("my.package.MyEnum"));
    }
}
