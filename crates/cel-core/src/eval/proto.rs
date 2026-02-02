//! Proto-to-CEL and CEL-to-proto conversions, struct construction, and WKT handling.

use std::collections::HashMap;
use std::sync::Arc;

use prost_reflect::{
    DynamicMessage, FieldDescriptor, Kind, MapKey as ProtoMapKey, MessageDescriptor,
    ReflectMessage,
};

use super::{
    wkt, EvalError, EvalErrorKind, MapKey, OptionalValue, Value, ValueMap,
};
use crate::types::SpannedExpr;

use super::evaluator::Evaluator;

impl<'a> Evaluator<'a> {
    /// Convert a prost_reflect Value to a CEL Value.
    pub(super) fn proto_reflect_to_value(
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
            prost_reflect::Value::EnumNumber(n) => {
                if let Kind::Enum(enum_desc) = field.kind() {
                    self.enum_or_int(enum_desc.full_name(), *n)
                } else {
                    Value::Int(*n as i64)
                }
            }
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
    pub(super) fn proto_scalar_to_value(&self, value: &prost_reflect::Value, kind: &Kind) -> Value {
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
            prost_reflect::Value::EnumNumber(n) => {
                if let Kind::Enum(enum_desc) = kind {
                    self.enum_or_int(enum_desc.full_name(), *n)
                } else {
                    Value::Int(*n as i64)
                }
            }
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
    pub(super) fn extension_value_to_cel(
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

    pub(super) fn eval_struct(
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
            "google.protobuf.FloatValue" => match &value {
                Value::Double(d) => Value::Double((*d as f32) as f64),
                Value::Int(i) => Value::Double((*i as f32) as f64),
                Value::UInt(u) => Value::Double((*u as f32) as f64),
                _ => Value::error(EvalError::type_mismatch("double", &value.cel_type().display_name())),
            },
            "google.protobuf.DoubleValue" => match &value {
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

        // Empty Any (no type_url) is a conversion error
        if type_url.is_empty() {
            return Value::error(EvalError::new(
                EvalErrorKind::InvalidConversion,
                "conversion",
            ));
        }

        // Auto-unpack Any to the contained message type
        self.maybe_unwrap_well_known(msg)
    }

    /// Set a field on a proto message, handling null values.
    /// Null is valid only for singular message-type fields (means "absent").
    /// For scalar, repeated, or map fields, null produces an error.
    pub(super) fn set_proto_field_or_null(
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
    pub(super) fn set_proto_field(
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
    pub(super) fn scalar_value_to_proto(
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
            (Value::Int(i), Kind::Enum(_)) => {
                if *i > i32::MAX as i64 || *i < i32::MIN as i64 {
                    Err(Value::error(EvalError::overflow("int to enum overflow")))
                } else {
                    Ok(prost_reflect::Value::EnumNumber(*i as i32))
                }
            }
            (Value::Enum(e), Kind::Enum(_)) => Ok(prost_reflect::Value::EnumNumber(e.value)),
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
    pub(super) fn maybe_unwrap_well_known(&self, message: DynamicMessage) -> Value {
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
}
