//! CEL type definitions.

/// CEL types that can be inferred from expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CelType {
    /// Boolean type
    Bool,
    /// Signed 64-bit integer
    Int,
    /// Unsigned 64-bit integer
    UInt,
    /// 64-bit floating point
    Double,
    /// Unicode string
    String,
    /// Byte sequence
    Bytes,
    /// Homogeneous list
    List,
    /// Key-value map
    Map,
    /// google.protobuf.Timestamp
    Timestamp,
    /// google.protobuf.Duration
    Duration,
    /// Null value
    Null,
    /// Dynamic/unknown type
    Dyn,
}

impl CelType {
    /// Returns the display name of this type as used in CEL.
    pub fn as_str(&self) -> &'static str {
        match self {
            CelType::Bool => "bool",
            CelType::Int => "int",
            CelType::UInt => "uint",
            CelType::Double => "double",
            CelType::String => "string",
            CelType::Bytes => "bytes",
            CelType::List => "list",
            CelType::Map => "map",
            CelType::Timestamp => "timestamp",
            CelType::Duration => "duration",
            CelType::Null => "null",
            CelType::Dyn => "dyn",
        }
    }
}
