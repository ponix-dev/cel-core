//! Well-known type helpers for CEL evaluation.
//!
//! Provides functions for unwrapping protobuf well-known types to native CEL values,
//! and for converting CEL values to well-known type messages (Value, Struct, ListValue, Any).

use std::sync::Arc;

use prost::Message;
use prost_reflect::{DynamicMessage, MessageDescriptor, ReflectMessage};

use super::{Duration, EvalError, MapKey, ProtoValue, Timestamp, Value, ValueMap};
use crate::types::ProtoTypeRegistry;

// ==================== Well-Known Type Unwrapping ====================

/// Check if a message is a well-known type and unwrap it to a native CEL value.
/// Non-WKT messages are returned as `Value::Proto`.
pub fn maybe_unwrap_well_known(message: DynamicMessage) -> Value {
    let descriptor = message.descriptor();
    let type_name = descriptor.full_name();
    match type_name {
        "google.protobuf.Timestamp" => {
            let seconds = get_field_i64(&message, "seconds").unwrap_or(0);
            let nanos = get_field_i32(&message, "nanos").unwrap_or(0);
            Value::Timestamp(Timestamp::new(seconds, nanos))
        }
        "google.protobuf.Duration" => {
            let seconds = get_field_i64(&message, "seconds").unwrap_or(0);
            let nanos = get_field_i32(&message, "nanos").unwrap_or(0);
            Value::Duration(Duration::new(seconds, nanos))
        }
        "google.protobuf.Int32Value" | "google.protobuf.Int64Value" => {
            let value = get_field_i64(&message, "value").unwrap_or(0);
            Value::Int(value)
        }
        "google.protobuf.UInt32Value" | "google.protobuf.UInt64Value" => {
            let value = get_field_u64(&message, "value").unwrap_or(0);
            Value::UInt(value)
        }
        "google.protobuf.FloatValue" | "google.protobuf.DoubleValue" => {
            let value = get_field_f64(&message, "value").unwrap_or(0.0);
            Value::Double(value)
        }
        "google.protobuf.BoolValue" => {
            let value = get_field_bool(&message, "value").unwrap_or(false);
            Value::Bool(value)
        }
        "google.protobuf.StringValue" => {
            let value = get_field_string(&message, "value").unwrap_or_default();
            Value::String(Arc::from(value))
        }
        "google.protobuf.BytesValue" => {
            let value = get_field_bytes(&message, "value").unwrap_or_default();
            Value::Bytes(Arc::from(value))
        }
        "google.protobuf.Value" => unwrap_google_value(&message),
        "google.protobuf.Struct" => unwrap_google_struct(&message),
        "google.protobuf.ListValue" => unwrap_google_list_value(&message),
        _ => Value::Proto(ProtoValue::new(message)),
    }
}

// ==================== Wrapper Type Checks ====================

/// Check if a message descriptor is any wrapper type.
pub fn is_wrapper_type(desc: &MessageDescriptor) -> bool {
    matches!(
        desc.full_name(),
        "google.protobuf.BoolValue"
            | "google.protobuf.Int32Value"
            | "google.protobuf.Int64Value"
            | "google.protobuf.UInt32Value"
            | "google.protobuf.UInt64Value"
            | "google.protobuf.FloatValue"
            | "google.protobuf.DoubleValue"
            | "google.protobuf.StringValue"
            | "google.protobuf.BytesValue"
    )
}

/// Check if a message descriptor is an int wrapper type.
pub fn is_int_wrapper(desc: &MessageDescriptor) -> bool {
    matches!(
        desc.full_name(),
        "google.protobuf.Int32Value" | "google.protobuf.Int64Value"
    )
}

/// Check if a message descriptor is a uint wrapper type.
pub fn is_uint_wrapper(desc: &MessageDescriptor) -> bool {
    matches!(
        desc.full_name(),
        "google.protobuf.UInt32Value" | "google.protobuf.UInt64Value"
    )
}

/// Check if a message descriptor is a double/float wrapper type.
pub fn is_double_wrapper(desc: &MessageDescriptor) -> bool {
    matches!(
        desc.full_name(),
        "google.protobuf.FloatValue" | "google.protobuf.DoubleValue"
    )
}

// ==================== Field Extraction Helpers ====================

/// Get an i64 field from a dynamic message.
pub fn get_field_i64(message: &DynamicMessage, field_name: &str) -> Option<i64> {
    let field = message.descriptor().get_field_by_name(field_name)?;
    match message.get_field(&field) {
        std::borrow::Cow::Owned(prost_reflect::Value::I64(v)) => Some(v),
        std::borrow::Cow::Borrowed(prost_reflect::Value::I64(v)) => Some(*v),
        std::borrow::Cow::Owned(prost_reflect::Value::I32(v)) => Some(v as i64),
        std::borrow::Cow::Borrowed(prost_reflect::Value::I32(v)) => Some(*v as i64),
        _ => None,
    }
}

/// Get an i32 field from a dynamic message.
pub fn get_field_i32(message: &DynamicMessage, field_name: &str) -> Option<i32> {
    let field = message.descriptor().get_field_by_name(field_name)?;
    match message.get_field(&field) {
        std::borrow::Cow::Owned(prost_reflect::Value::I32(v)) => Some(v),
        std::borrow::Cow::Borrowed(prost_reflect::Value::I32(v)) => Some(*v),
        std::borrow::Cow::Owned(prost_reflect::Value::I64(v)) => Some(v as i32),
        std::borrow::Cow::Borrowed(prost_reflect::Value::I64(v)) => Some(*v as i32),
        _ => None,
    }
}

/// Get a u64 field from a dynamic message.
pub fn get_field_u64(message: &DynamicMessage, field_name: &str) -> Option<u64> {
    let field = message.descriptor().get_field_by_name(field_name)?;
    match message.get_field(&field) {
        std::borrow::Cow::Owned(prost_reflect::Value::U64(v)) => Some(v),
        std::borrow::Cow::Borrowed(prost_reflect::Value::U64(v)) => Some(*v),
        std::borrow::Cow::Owned(prost_reflect::Value::U32(v)) => Some(v as u64),
        std::borrow::Cow::Borrowed(prost_reflect::Value::U32(v)) => Some(*v as u64),
        _ => None,
    }
}

/// Get an f64 field from a dynamic message.
pub fn get_field_f64(message: &DynamicMessage, field_name: &str) -> Option<f64> {
    let field = message.descriptor().get_field_by_name(field_name)?;
    match message.get_field(&field) {
        std::borrow::Cow::Owned(prost_reflect::Value::F64(v)) => Some(v),
        std::borrow::Cow::Borrowed(prost_reflect::Value::F64(v)) => Some(*v),
        std::borrow::Cow::Owned(prost_reflect::Value::F32(v)) => Some(v as f64),
        std::borrow::Cow::Borrowed(prost_reflect::Value::F32(v)) => Some(*v as f64),
        _ => None,
    }
}

/// Get a bool field from a dynamic message.
pub fn get_field_bool(message: &DynamicMessage, field_name: &str) -> Option<bool> {
    let field = message.descriptor().get_field_by_name(field_name)?;
    match message.get_field(&field) {
        std::borrow::Cow::Owned(prost_reflect::Value::Bool(v)) => Some(v),
        std::borrow::Cow::Borrowed(prost_reflect::Value::Bool(v)) => Some(*v),
        _ => None,
    }
}

/// Get a string field from a dynamic message.
pub fn get_field_string(message: &DynamicMessage, field_name: &str) -> Option<String> {
    let field = message.descriptor().get_field_by_name(field_name)?;
    match message.get_field(&field) {
        std::borrow::Cow::Owned(prost_reflect::Value::String(v)) => Some(v),
        std::borrow::Cow::Borrowed(prost_reflect::Value::String(v)) => Some(v.clone()),
        _ => None,
    }
}

/// Get a bytes field from a dynamic message.
pub fn get_field_bytes(message: &DynamicMessage, field_name: &str) -> Option<Vec<u8>> {
    let field = message.descriptor().get_field_by_name(field_name)?;
    match message.get_field(&field) {
        std::borrow::Cow::Owned(prost_reflect::Value::Bytes(v)) => Some(v.to_vec()),
        std::borrow::Cow::Borrowed(prost_reflect::Value::Bytes(v)) => Some(v.to_vec()),
        _ => None,
    }
}

// ==================== google.protobuf.Value/Struct/ListValue Unwrapping ====================

/// Unwrap a `google.protobuf.Value` message to a native CEL value.
fn unwrap_google_value(message: &DynamicMessage) -> Value {
    let descriptor = message.descriptor();
    // The Value message has a `kind` oneof with fields:
    // null_value, number_value, string_value, bool_value, struct_value, list_value
    if let Some(field) = descriptor.get_field_by_name("null_value") {
        if message.has_field(&field) {
            return Value::Null;
        }
    }
    if let Some(field) = descriptor.get_field_by_name("bool_value") {
        if message.has_field(&field) {
            let val = message.get_field(&field);
            if let prost_reflect::Value::Bool(b) = val.as_ref() {
                return Value::Bool(*b);
            }
        }
    }
    if let Some(field) = descriptor.get_field_by_name("number_value") {
        if message.has_field(&field) {
            let val = message.get_field(&field);
            if let prost_reflect::Value::F64(d) = val.as_ref() {
                return Value::Double(*d);
            }
        }
    }
    if let Some(field) = descriptor.get_field_by_name("string_value") {
        if message.has_field(&field) {
            let val = message.get_field(&field);
            if let prost_reflect::Value::String(s) = val.as_ref() {
                return Value::String(Arc::from(s.as_str()));
            }
        }
    }
    if let Some(field) = descriptor.get_field_by_name("struct_value") {
        if message.has_field(&field) {
            let val = message.get_field(&field);
            if let prost_reflect::Value::Message(msg) = val.as_ref() {
                return unwrap_google_struct(msg);
            }
        }
    }
    if let Some(field) = descriptor.get_field_by_name("list_value") {
        if message.has_field(&field) {
            let val = message.get_field(&field);
            if let prost_reflect::Value::Message(msg) = val.as_ref() {
                return unwrap_google_list_value(msg);
            }
        }
    }
    // No oneof field set - treat as null (default for Value)
    Value::Null
}

/// Unwrap a `google.protobuf.Struct` message to a CEL map value.
fn unwrap_google_struct(message: &DynamicMessage) -> Value {
    let descriptor = message.descriptor();
    if let Some(fields_field) = descriptor.get_field_by_name("fields") {
        let val = message.get_field(&fields_field);
        if let prost_reflect::Value::Map(map) = val.as_ref() {
            let mut value_map = ValueMap::new();
            for (k, v) in map {
                if let prost_reflect::MapKey::String(key_str) = k {
                    let cel_value = if let prost_reflect::Value::Message(msg) = v {
                        unwrap_google_value(msg)
                    } else {
                        Value::Null
                    };
                    value_map.insert(MapKey::String(Arc::from(key_str.as_str())), cel_value);
                }
            }
            return Value::Map(Arc::new(value_map));
        }
    }
    // Empty struct
    Value::Map(Arc::new(ValueMap::new()))
}

/// Unwrap a `google.protobuf.ListValue` message to a CEL list value.
fn unwrap_google_list_value(message: &DynamicMessage) -> Value {
    let descriptor = message.descriptor();
    if let Some(values_field) = descriptor.get_field_by_name("values") {
        let val = message.get_field(&values_field);
        if let prost_reflect::Value::List(list) = val.as_ref() {
            let values: Vec<Value> = list
                .iter()
                .map(|v| {
                    if let prost_reflect::Value::Message(msg) = v {
                        unwrap_google_value(msg)
                    } else {
                        Value::Null
                    }
                })
                .collect();
            return Value::List(Arc::from(values));
        }
    }
    // Empty list
    Value::List(Arc::from(Vec::<Value>::new()))
}

// ==================== WKT Coercion (CEL value → google.protobuf.*) ====================

/// Convert a CEL value to a `google.protobuf.Value` message.
pub fn cel_value_to_google_value(
    value: &Value,
    registry: &ProtoTypeRegistry,
) -> Result<DynamicMessage, Value> {
    let value_desc = registry
        .get_message("google.protobuf.Value")
        .ok_or_else(|| Value::error(EvalError::internal("google.protobuf.Value not in registry")))?;

    let mut msg = DynamicMessage::new(value_desc.clone());

    match value {
        Value::Null => {
            if let Some(field) = value_desc.get_field_by_name("null_value") {
                msg.set_field(&field, prost_reflect::Value::EnumNumber(0));
            }
        }
        Value::Bool(b) => {
            if let Some(field) = value_desc.get_field_by_name("bool_value") {
                msg.set_field(&field, prost_reflect::Value::Bool(*b));
            }
        }
        Value::Int(i) => {
            if let Some(field) = value_desc.get_field_by_name("number_value") {
                msg.set_field(&field, prost_reflect::Value::F64(*i as f64));
            }
        }
        Value::UInt(u) => {
            if let Some(field) = value_desc.get_field_by_name("number_value") {
                msg.set_field(&field, prost_reflect::Value::F64(*u as f64));
            }
        }
        Value::Double(d) => {
            if let Some(field) = value_desc.get_field_by_name("number_value") {
                msg.set_field(&field, prost_reflect::Value::F64(*d));
            }
        }
        Value::String(s) => {
            if let Some(field) = value_desc.get_field_by_name("string_value") {
                msg.set_field(&field, prost_reflect::Value::String(s.to_string()));
            }
        }
        Value::List(list) => {
            let list_msg = cel_list_to_list_value(list, registry)?;
            if let Some(field) = value_desc.get_field_by_name("list_value") {
                msg.set_field(&field, prost_reflect::Value::Message(list_msg));
            }
        }
        Value::Map(map) => {
            let struct_msg = cel_map_to_struct(map, registry)?;
            if let Some(field) = value_desc.get_field_by_name("struct_value") {
                msg.set_field(&field, prost_reflect::Value::Message(struct_msg));
            }
        }
        _ => {
            return Err(Value::error(EvalError::type_mismatch(
                "google.protobuf.Value",
                &value.cel_type().display_name(),
            )));
        }
    }

    Ok(msg)
}

/// Convert a CEL map (string-keyed) to a `google.protobuf.Struct` message.
pub fn cel_map_to_struct(
    map: &ValueMap,
    registry: &ProtoTypeRegistry,
) -> Result<DynamicMessage, Value> {
    let struct_desc = registry
        .get_message("google.protobuf.Struct")
        .ok_or_else(|| Value::error(EvalError::internal("google.protobuf.Struct not in registry")))?;

    let mut msg = DynamicMessage::new(struct_desc.clone());

    if let Some(fields_field) = struct_desc.get_field_by_name("fields") {
        let mut proto_map = std::collections::HashMap::new();
        for (key, value) in map.iter() {
            let key_str = match key {
                MapKey::String(s) => s.to_string(),
                _ => {
                    return Err(Value::error(EvalError::type_mismatch(
                        "string key for Struct",
                        &key.to_value().cel_type().display_name(),
                    )));
                }
            };
            let value_msg = cel_value_to_google_value(value, registry)?;
            proto_map.insert(
                prost_reflect::MapKey::String(key_str),
                prost_reflect::Value::Message(value_msg),
            );
        }
        msg.set_field(&fields_field, prost_reflect::Value::Map(proto_map));
    }

    Ok(msg)
}

/// Convert a CEL list to a `google.protobuf.ListValue` message.
pub fn cel_list_to_list_value(
    list: &[Value],
    registry: &ProtoTypeRegistry,
) -> Result<DynamicMessage, Value> {
    let list_desc = registry
        .get_message("google.protobuf.ListValue")
        .ok_or_else(|| {
            Value::error(EvalError::internal(
                "google.protobuf.ListValue not in registry",
            ))
        })?;

    let mut msg = DynamicMessage::new(list_desc.clone());

    if let Some(values_field) = list_desc.get_field_by_name("values") {
        let mut proto_values = Vec::with_capacity(list.len());
        for item in list {
            let value_msg = cel_value_to_google_value(item, registry)?;
            proto_values.push(prost_reflect::Value::Message(value_msg));
        }
        msg.set_field(&values_field, prost_reflect::Value::List(proto_values));
    }

    Ok(msg)
}

/// Pack a DynamicMessage into a `google.protobuf.Any` message.
pub fn pack_message_into_any(
    source: &DynamicMessage,
    any_desc: &MessageDescriptor,
) -> Result<prost_reflect::Value, Value> {
    let mut any_msg = DynamicMessage::new(any_desc.clone());

    let type_url = format!(
        "type.googleapis.com/{}",
        source.descriptor().full_name()
    );
    let encoded = source.encode_to_vec();

    if let Some(type_url_field) = any_desc.get_field_by_name("type_url") {
        any_msg.set_field(&type_url_field, prost_reflect::Value::String(type_url));
    }
    if let Some(value_field) = any_desc.get_field_by_name("value") {
        any_msg.set_field(
            &value_field,
            prost_reflect::Value::Bytes(prost::bytes::Bytes::from(encoded)),
        );
    }

    Ok(prost_reflect::Value::Message(any_msg))
}

/// Wrap a CEL primitive value in its corresponding wrapper type message,
/// then pack into Any.
pub fn wrap_value_for_any(
    value: &Value,
    registry: &ProtoTypeRegistry,
) -> Result<DynamicMessage, Value> {
    let any_desc = registry
        .get_message("google.protobuf.Any")
        .ok_or_else(|| Value::error(EvalError::internal("google.protobuf.Any not in registry")))?;

    // Determine which wrapper type to use and create the wrapper message
    let wrapper_msg = match value {
        Value::Bool(b) => {
            let desc = registry.get_message("google.protobuf.BoolValue").ok_or_else(|| {
                Value::error(EvalError::internal("BoolValue not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("value") {
                msg.set_field(&f, prost_reflect::Value::Bool(*b));
            }
            msg
        }
        Value::Int(i) => {
            let desc = registry.get_message("google.protobuf.Int64Value").ok_or_else(|| {
                Value::error(EvalError::internal("Int64Value not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("value") {
                msg.set_field(&f, prost_reflect::Value::I64(*i));
            }
            msg
        }
        Value::UInt(u) => {
            let desc = registry.get_message("google.protobuf.UInt64Value").ok_or_else(|| {
                Value::error(EvalError::internal("UInt64Value not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("value") {
                msg.set_field(&f, prost_reflect::Value::U64(*u));
            }
            msg
        }
        Value::Double(d) => {
            let desc = registry.get_message("google.protobuf.DoubleValue").ok_or_else(|| {
                Value::error(EvalError::internal("DoubleValue not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("value") {
                msg.set_field(&f, prost_reflect::Value::F64(*d));
            }
            msg
        }
        Value::String(s) => {
            let desc = registry.get_message("google.protobuf.StringValue").ok_or_else(|| {
                Value::error(EvalError::internal("StringValue not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("value") {
                msg.set_field(&f, prost_reflect::Value::String(s.to_string()));
            }
            msg
        }
        Value::Bytes(b) => {
            let desc = registry.get_message("google.protobuf.BytesValue").ok_or_else(|| {
                Value::error(EvalError::internal("BytesValue not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("value") {
                msg.set_field(
                    &f,
                    prost_reflect::Value::Bytes(prost::bytes::Bytes::copy_from_slice(b)),
                );
            }
            msg
        }
        Value::Timestamp(ts) => {
            let desc = registry.get_message("google.protobuf.Timestamp").ok_or_else(|| {
                Value::error(EvalError::internal("Timestamp not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("seconds") {
                msg.set_field(&f, prost_reflect::Value::I64(ts.seconds));
            }
            if let Some(f) = desc.get_field_by_name("nanos") {
                msg.set_field(&f, prost_reflect::Value::I32(ts.nanos));
            }
            msg
        }
        Value::Duration(d) => {
            let desc = registry.get_message("google.protobuf.Duration").ok_or_else(|| {
                Value::error(EvalError::internal("Duration not in registry"))
            })?;
            let mut msg = DynamicMessage::new(desc.clone());
            if let Some(f) = desc.get_field_by_name("seconds") {
                msg.set_field(&f, prost_reflect::Value::I64(d.seconds));
            }
            if let Some(f) = desc.get_field_by_name("nanos") {
                msg.set_field(&f, prost_reflect::Value::I32(d.nanos));
            }
            msg
        }
        Value::Map(map) => cel_map_to_struct(map, registry)?,
        Value::List(list) => cel_list_to_list_value(list, registry)?,
        Value::Null => {
            // Null wraps as google.protobuf.Value with null_value
            cel_value_to_google_value(value, registry)?
        }
        _ => {
            return Err(Value::error(EvalError::type_mismatch(
                "Any-wrappable value",
                &value.cel_type().display_name(),
            )));
        }
    };

    // Pack the wrapper message into Any
    let mut any_msg = DynamicMessage::new(any_desc.clone());
    let type_url = format!(
        "type.googleapis.com/{}",
        wrapper_msg.descriptor().full_name()
    );
    let encoded = wrapper_msg.encode_to_vec();

    if let Some(type_url_field) = any_desc.get_field_by_name("type_url") {
        any_msg.set_field(&type_url_field, prost_reflect::Value::String(type_url));
    }
    if let Some(value_field) = any_desc.get_field_by_name("value") {
        any_msg.set_field(
            &value_field,
            prost_reflect::Value::Bytes(prost::bytes::Bytes::from(encoded)),
        );
    }

    Ok(any_msg)
}
