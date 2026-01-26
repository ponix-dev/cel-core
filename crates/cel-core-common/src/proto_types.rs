//! Proto type registry for resolving protobuf types during type checking.
//!
//! This module provides `ProtoTypeRegistry`, which wraps a `prost_reflect::DescriptorPool`
//! to enable field type lookup, enum value resolution, and well-known type mapping.

use prost_reflect::prost::Message;
use prost_reflect::{DescriptorPool, EnumDescriptor, FieldDescriptor, Kind, MessageDescriptor};

use crate::CelType;

/// Registry for protobuf type information.
///
/// Wraps a `prost_reflect::DescriptorPool` to provide:
/// - Message field type lookup
/// - Enum value resolution
/// - Nested type resolution
/// - Well-known type mapping
#[derive(Debug, Clone)]
pub struct ProtoTypeRegistry {
    pool: DescriptorPool,
}

/// Result of resolving a qualified proto name.
#[derive(Debug, Clone)]
pub enum ResolvedProtoType {
    /// Resolved to a message type.
    Message {
        /// Fully qualified message name.
        name: String,
        /// CEL type representation.
        cel_type: CelType,
    },
    /// Resolved to an enum type.
    Enum {
        /// Fully qualified enum name.
        name: String,
        /// CEL type representation.
        cel_type: CelType,
    },
    /// Resolved to an enum value.
    EnumValue {
        /// Fully qualified enum name.
        enum_name: String,
        /// Numeric value of the enum constant.
        value: i32,
    },
}

impl ProtoTypeRegistry {
    /// Create a new proto type registry with well-known types pre-loaded.
    pub fn new() -> Self {
        // Start with the global pool which includes well-known types
        // (google.protobuf.Timestamp, Duration, Any, etc.)
        Self {
            pool: DescriptorPool::global(),
        }
    }

    /// Create a registry from an existing descriptor pool.
    pub fn from_pool(pool: DescriptorPool) -> Self {
        Self { pool }
    }

    /// Add file descriptors to the registry.
    ///
    /// The bytes should be serialized `FileDescriptorSet` proto.
    /// Any duplicates of files already in the pool will be skipped.
    pub fn add_file_descriptor_set(&mut self, bytes: &[u8]) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let fds = prost_reflect::prost_types::FileDescriptorSet::decode(bytes)?;
        self.pool.add_file_descriptor_set(fds)?;
        Ok(())
    }

    /// Get a message descriptor by fully qualified name.
    pub fn get_message(&self, name: &str) -> Option<MessageDescriptor> {
        self.pool.get_message_by_name(name)
    }

    /// Get an enum descriptor by fully qualified name.
    pub fn get_enum(&self, name: &str) -> Option<EnumDescriptor> {
        self.pool.get_enum_by_name(name)
    }

    /// Get the CEL type of a field in a message.
    pub fn get_field_type(&self, message_name: &str, field_name: &str) -> Option<CelType> {
        let message = self.get_message(message_name)?;
        let field = message.get_field_by_name(field_name)?;
        Some(self.field_to_cel_type(&field))
    }

    /// Get an enum value by enum name and value name.
    pub fn get_enum_value(&self, enum_name: &str, value_name: &str) -> Option<i32> {
        let enum_desc = self.get_enum(enum_name)?;
        let value = enum_desc.get_value_by_name(value_name)?;
        Some(value.number())
    }

    /// Resolve a qualified name (e.g., `TestAllTypes.NestedEnum.BAR`).
    ///
    /// This handles:
    /// - Message types
    /// - Enum types
    /// - Enum values (e.g., `GlobalEnum.GAZ` -> enum value)
    pub fn resolve_qualified(&self, parts: &[&str], container: &str) -> Option<ResolvedProtoType> {
        if parts.is_empty() {
            return None;
        }

        // Build candidate names to try
        let mut candidates = Vec::new();

        // Try with container prefix first
        if !container.is_empty() {
            candidates.push(format!("{}.{}", container, parts.join(".")));

            // Also try container prefixes for shorter paths
            let mut container_parts: Vec<&str> = container.split('.').collect();
            while !container_parts.is_empty() {
                candidates.push(format!("{}.{}", container_parts.join("."), parts.join(".")));
                container_parts.pop();
            }
        }

        // Try without container
        candidates.push(parts.join("."));

        // Try each candidate as a message or enum
        for candidate in &candidates {
            // Try as message
            if let Some(msg) = self.get_message(candidate) {
                return Some(ResolvedProtoType::Message {
                    name: msg.full_name().to_string(),
                    cel_type: proto_message_to_cel_type(msg.full_name()),
                });
            }

            // Try as enum
            if let Some(enum_desc) = self.get_enum(candidate) {
                return Some(ResolvedProtoType::Enum {
                    name: enum_desc.full_name().to_string(),
                    cel_type: CelType::enum_type(enum_desc.full_name()),
                });
            }
        }

        // Try as enum value: last part is value name, rest is enum name
        if parts.len() >= 2 {
            let value_name = parts[parts.len() - 1];
            let enum_parts = &parts[..parts.len() - 1];

            let mut enum_candidates = Vec::new();
            if !container.is_empty() {
                enum_candidates.push(format!("{}.{}", container, enum_parts.join(".")));
                let mut container_parts: Vec<&str> = container.split('.').collect();
                while !container_parts.is_empty() {
                    enum_candidates.push(format!("{}.{}", container_parts.join("."), enum_parts.join(".")));
                    container_parts.pop();
                }
            }
            enum_candidates.push(enum_parts.join("."));

            for enum_name in &enum_candidates {
                if let Some(value) = self.get_enum_value(enum_name, value_name) {
                    return Some(ResolvedProtoType::EnumValue {
                        enum_name: enum_name.clone(),
                        value,
                    });
                }
            }
        }

        None
    }

    /// Resolve a message name to its fully qualified form.
    ///
    /// Tries the name as-is first, then with container prefix.
    pub fn resolve_message_name(&self, name: &str, container: &str) -> Option<String> {
        // Try with container prefix first
        if !container.is_empty() {
            let qualified = format!("{}.{}", container, name);
            if self.get_message(&qualified).is_some() {
                return Some(qualified);
            }

            // Try container prefixes
            let mut container_parts: Vec<&str> = container.split('.').collect();
            while !container_parts.is_empty() {
                let qualified = format!("{}.{}", container_parts.join("."), name);
                if self.get_message(&qualified).is_some() {
                    return Some(qualified);
                }
                container_parts.pop();
            }
        }

        // Try as-is
        if self.get_message(name).is_some() {
            return Some(name.to_string());
        }

        None
    }

    /// Convert a field descriptor to a CEL type.
    fn field_to_cel_type(&self, field: &FieldDescriptor) -> CelType {
        let base_type = self.kind_to_cel_type(field.kind());

        if field.is_list() {
            CelType::list(base_type)
        } else if field.is_map() {
            // For map fields, get the key and value types
            if let Kind::Message(map_entry) = field.kind() {
                let key_field = map_entry.get_field_by_name("key");
                let value_field = map_entry.get_field_by_name("value");

                let key_type = key_field
                    .map(|f| self.kind_to_cel_type(f.kind()))
                    .unwrap_or(CelType::Dyn);
                let value_type = value_field
                    .map(|f| self.kind_to_cel_type(f.kind()))
                    .unwrap_or(CelType::Dyn);

                CelType::map(key_type, value_type)
            } else {
                CelType::map(CelType::Dyn, CelType::Dyn)
            }
        } else {
            base_type
        }
    }

    /// Convert a proto Kind to a CEL type.
    fn kind_to_cel_type(&self, kind: Kind) -> CelType {
        match kind {
            Kind::Bool => CelType::Bool,
            Kind::Int32 | Kind::Sint32 | Kind::Sfixed32 | Kind::Int64 | Kind::Sint64 | Kind::Sfixed64 => {
                CelType::Int
            }
            Kind::Uint32 | Kind::Fixed32 | Kind::Uint64 | Kind::Fixed64 => CelType::UInt,
            Kind::Float | Kind::Double => CelType::Double,
            Kind::String => CelType::String,
            Kind::Bytes => CelType::Bytes,
            Kind::Message(msg) => proto_message_to_cel_type(msg.full_name()),
            Kind::Enum(_) => CelType::Int, // Enum values are ints in CEL
        }
    }

    /// Get the underlying descriptor pool.
    pub fn pool(&self) -> &DescriptorPool {
        &self.pool
    }
}

impl Default for ProtoTypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert a proto message type name to its CEL type representation.
///
/// This handles well-known types specially:
/// - `google.protobuf.Timestamp` -> `CelType::Timestamp`
/// - `google.protobuf.Duration` -> `CelType::Duration`
/// - Wrapper types -> `CelType::Wrapper(inner)`
/// - Other messages -> `CelType::Message(name)`
pub fn proto_message_to_cel_type(full_name: &str) -> CelType {
    match full_name {
        // Well-known types
        "google.protobuf.Timestamp" => CelType::Timestamp,
        "google.protobuf.Duration" => CelType::Duration,

        // Wrapper types
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

        // Any, Struct, Value, ListValue
        "google.protobuf.Any"
        | "google.protobuf.Struct"
        | "google.protobuf.Value"
        | "google.protobuf.ListValue" => CelType::Dyn,

        // Regular message types
        _ => CelType::message(full_name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_well_known_type_mapping() {
        assert_eq!(
            proto_message_to_cel_type("google.protobuf.Timestamp"),
            CelType::Timestamp
        );
        assert_eq!(
            proto_message_to_cel_type("google.protobuf.Duration"),
            CelType::Duration
        );
        assert_eq!(
            proto_message_to_cel_type("google.protobuf.Int64Value"),
            CelType::wrapper(CelType::Int)
        );
    }

    #[test]
    fn test_regular_message_type() {
        assert_eq!(
            proto_message_to_cel_type("my.package.MyMessage"),
            CelType::message("my.package.MyMessage")
        );
    }
}
