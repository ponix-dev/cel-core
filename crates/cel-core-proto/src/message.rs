//! Prost-based implementation of `MessageValue`.
//!
//! `ProstMessage` wraps a `prost_reflect::DynamicMessage` and implements
//! the `MessageValue` trait from cel-core, decoupling the proto runtime
//! from the core evaluation engine.

use std::sync::Arc;

use prost_reflect::{DynamicMessage, ReflectMessage};

use cel_core::eval::message::MessageValue;

/// A protobuf message value backed by prost-reflect.
///
/// Wraps a `prost_reflect::DynamicMessage` for runtime proto handling.
/// Uses Arc for cheap cloning.
#[derive(Clone)]
pub struct ProstMessage {
    /// The dynamic message instance.
    message: Arc<DynamicMessage>,
    /// Cached type name for efficient access.
    type_name: Arc<str>,
}

impl ProstMessage {
    /// Create a new prost message value from a dynamic message.
    pub fn new(message: DynamicMessage) -> Self {
        let type_name = Arc::from(message.descriptor().full_name());
        Self {
            message: Arc::new(message),
            type_name,
        }
    }

    /// Get a reference to the underlying dynamic message.
    pub fn message(&self) -> &DynamicMessage {
        &self.message
    }

    /// Get the fully qualified type name of this message.
    pub fn type_name(&self) -> &str {
        &self.type_name
    }

    /// Get the message descriptor.
    pub fn descriptor(&self) -> prost_reflect::MessageDescriptor {
        self.message.descriptor()
    }
}

impl std::fmt::Debug for ProstMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ProstMessage")
            .field("type_name", &self.type_name())
            .finish()
    }
}

impl MessageValue for ProstMessage {
    fn type_name(&self) -> &str {
        &self.type_name
    }

    fn eq_message(&self, other: &dyn MessageValue) -> bool {
        if let Some(other_proto) = other.as_any().downcast_ref::<ProstMessage>() {
            self == other_proto
        } else {
            false
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn clone_boxed(&self) -> Box<dyn MessageValue> {
        Box::new(self.clone())
    }

    fn is_default(&self) -> Option<bool> {
        Some(self.message
            .descriptor()
            .fields()
            .all(|field_desc| !self.message.has_field(&field_desc)))
    }
}

impl PartialEq for ProstMessage {
    fn eq(&self, other: &Self) -> bool {
        // Messages are equal if same type and all fields equal
        if self.type_name() != other.type_name() {
            return false;
        }
        // For Any messages, compare by unpacking to the inner message
        if self.type_name() == "google.protobuf.Any" {
            return any_semantic_eq(&self.message, &other.message);
        }
        // Use prost_reflect's equality
        self.message.as_ref() == other.message.as_ref()
    }
}

/// Compare two `google.protobuf.Any` messages by semantic equality.
///
/// Unpacks both Any messages to their inner message type and compares
/// the decoded messages. Falls back to bytewise comparison if decoding fails.
fn any_semantic_eq(
    a: &prost_reflect::DynamicMessage,
    b: &prost_reflect::DynamicMessage,
) -> bool {
    let descriptor = a.descriptor();

    // Extract type_url from both
    let type_url_field = match descriptor.get_field_by_name("type_url") {
        Some(f) => f,
        None => return a == b,
    };
    let value_field = match descriptor.get_field_by_name("value") {
        Some(f) => f,
        None => return a == b,
    };

    let type_url_a = match a.get_field(&type_url_field).into_owned() {
        prost_reflect::Value::String(s) => s,
        _ => return a == b,
    };
    let type_url_b = match b.get_field(&type_url_field).into_owned() {
        prost_reflect::Value::String(s) => s,
        _ => return a == b,
    };

    // Different type_urls means not equal
    if type_url_a != type_url_b {
        return false;
    }

    // Strip the prefix to get the message type name
    let type_name = type_url_a
        .strip_prefix("type.googleapis.com/")
        .unwrap_or(&type_url_a);

    // Look up the inner message descriptor
    let inner_descriptor = match descriptor.parent_pool().get_message_by_name(type_name) {
        Some(d) => d,
        None => {
            // Can't resolve type -- fall back to bytewise comparison of value bytes
            return a.get_field(&value_field) == b.get_field(&value_field);
        }
    };

    // Extract value bytes from both
    let bytes_a = match a.get_field(&value_field).into_owned() {
        prost_reflect::Value::Bytes(b) => b,
        _ => return a == b,
    };
    let bytes_b = match b.get_field(&value_field).into_owned() {
        prost_reflect::Value::Bytes(b) => b,
        _ => return a == b,
    };

    // Decode both into DynamicMessage and compare semantically
    let msg_a = match prost_reflect::DynamicMessage::decode(inner_descriptor.clone(), bytes_a.as_ref()) {
        Ok(m) => m,
        Err(_) => return bytes_a == bytes_b,
    };
    let msg_b = match prost_reflect::DynamicMessage::decode(inner_descriptor, bytes_b.as_ref()) {
        Ok(m) => m,
        Err(_) => return bytes_a == bytes_b,
    };

    msg_a == msg_b
}
