//! Trait abstraction for protobuf type registries.
//!
//! `TypeRegistry` defines the interface for resolving protobuf types,
//! constructing messages, and accessing message fields, decoupling
//! cel-core from any specific protobuf implementation.

use std::any::Any;
use std::fmt;

use super::message::MessageValue;
use super::Value;
use crate::types::{CelType, ResolvedProtoType};

/// An evaluated struct field ready for message construction.
#[derive(Debug, Clone)]
pub struct StructFieldValue {
    /// The field name.
    pub name: String,
    /// The evaluated value.
    pub value: Value,
    /// Whether this is an optional field entry.
    pub optional: bool,
}

/// A trait representing a protobuf type registry.
///
/// This abstraction allows cel-core's checker and evaluator to work with
/// proto types without depending on a specific protobuf library.
pub trait TypeRegistry: fmt::Debug + Send + Sync {
    // ==================== Checker Methods ====================

    /// Get the CEL type of a message field.
    fn get_field_type(&self, message: &str, field: &str) -> Option<CelType>;

    /// Get the numeric value of an enum constant by name.
    fn get_enum_value(&self, enum_name: &str, value_name: &str) -> Option<i32>;

    /// Resolve a qualified name (dot-separated parts) within a container namespace.
    ///
    /// Returns the resolved proto type (Message, Enum, or EnumValue).
    fn resolve_qualified(&self, parts: &[&str], container: &str) -> Option<ResolvedProtoType>;

    /// Resolve a message name within a container namespace using C++ namespace rules.
    ///
    /// Returns the fully qualified message name if found.
    fn resolve_message_name(&self, name: &str, container: &str) -> Option<String>;

    // ==================== Evaluator Methods ====================

    /// Construct a protobuf message from evaluated field values.
    ///
    /// Returns the constructed message as a Value (may be unwrapped to a
    /// native CEL value for well-known types like Timestamp, Duration, etc.).
    fn construct_message(
        &self,
        type_name: &str,
        fields: &[StructFieldValue],
        strong_enums: bool,
    ) -> Value;

    /// Access a field on a message value.
    ///
    /// Returns the field value, or an error/optional-none for missing fields.
    fn message_field_access(
        &self,
        msg: &dyn MessageValue,
        field: &str,
        optional: bool,
        strong_enums: bool,
    ) -> Value;

    /// Test whether a message has a field set (for `has()` macro).
    fn message_has_field(&self, msg: &dyn MessageValue, field: &str) -> Value;

    /// Get an extension field value from a message.
    ///
    /// Returns `None` if the extension is not applicable to this message type.
    fn get_extension_value(
        &self,
        msg: &dyn MessageValue,
        ext_name: &str,
        optional: bool,
        strong_enums: bool,
    ) -> Option<Value>;

    /// Test whether a message has an extension field set.
    ///
    /// Returns `None` if the extension is not applicable to this message type.
    fn has_extension(&self, msg: &dyn MessageValue, ext_name: &str) -> Option<bool>;

    /// Downcast to a concrete type via `Any`.
    fn as_any(&self) -> &dyn Any;
}
