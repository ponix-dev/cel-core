//! Runtime values for CEL evaluation.
//!
//! `Value` represents all CEL values at runtime, including primitive types,
//! collections, timestamps, durations, and special values like errors and optionals.

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

use super::EvalError;
use crate::types::CelType;

/// A CEL runtime value.
///
/// This enum represents all possible values that can exist during CEL evaluation.
/// Unlike `CelValue` (which represents compile-time constants), `Value` supports
/// the full range of CEL types including collections, timestamps, and errors.
#[derive(Debug, Clone)]
pub enum Value {
    /// Null value.
    Null,
    /// Boolean value.
    Bool(bool),
    /// Signed 64-bit integer.
    Int(i64),
    /// Unsigned 64-bit integer.
    UInt(u64),
    /// 64-bit floating point.
    Double(f64),
    /// Unicode string (Arc for cheap cloning).
    String(Arc<str>),
    /// Byte sequence (Arc for cheap cloning).
    Bytes(Arc<[u8]>),
    /// Homogeneous list.
    List(Arc<[Value]>),
    /// Key-value map (uses BTreeMap for deterministic iteration).
    Map(Arc<ValueMap>),
    /// Timestamp (seconds and nanos since Unix epoch).
    Timestamp(Timestamp),
    /// Duration (seconds and nanos).
    Duration(Duration),
    /// Type value (represents a CEL type at runtime).
    Type(TypeValue),
    /// Optional value (present or absent).
    Optional(OptionalValue),
    /// Error value (evaluation errors propagate as values).
    Error(Arc<EvalError>),
}

/// A CEL timestamp value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Timestamp {
    /// Seconds since Unix epoch.
    pub seconds: i64,
    /// Nanoseconds (0..999_999_999).
    pub nanos: i32,
}

impl Timestamp {
    /// Create a new timestamp.
    pub fn new(seconds: i64, nanos: i32) -> Self {
        Self { seconds, nanos }
    }

    /// Create a timestamp from seconds since Unix epoch.
    pub fn from_seconds(seconds: i64) -> Self {
        Self { seconds, nanos: 0 }
    }

    /// Returns true if this timestamp is before another.
    pub fn is_before(&self, other: &Timestamp) -> bool {
        (self.seconds, self.nanos) < (other.seconds, other.nanos)
    }

    /// Returns true if this timestamp is after another.
    pub fn is_after(&self, other: &Timestamp) -> bool {
        (self.seconds, self.nanos) > (other.seconds, other.nanos)
    }
}

/// A CEL duration value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Duration {
    /// Seconds component.
    pub seconds: i64,
    /// Nanoseconds component (0..999_999_999 for positive durations,
    /// -999_999_999..0 for negative durations).
    pub nanos: i32,
}

impl Duration {
    /// Create a new duration.
    pub fn new(seconds: i64, nanos: i32) -> Self {
        Self { seconds, nanos }
    }

    /// Create a duration from seconds.
    pub fn from_seconds(seconds: i64) -> Self {
        Self { seconds, nanos: 0 }
    }

    /// Create a duration from nanoseconds.
    pub fn from_nanos(nanos: i64) -> Self {
        let seconds = nanos / 1_000_000_000;
        let nanos = (nanos % 1_000_000_000) as i32;
        Self { seconds, nanos }
    }

    /// Convert to total nanoseconds.
    pub fn to_nanos(&self) -> i64 {
        self.seconds
            .saturating_mul(1_000_000_000)
            .saturating_add(self.nanos as i64)
    }

    /// Returns true if this duration is negative.
    pub fn is_negative(&self) -> bool {
        self.seconds < 0 || (self.seconds == 0 && self.nanos < 0)
    }
}

/// A CEL type value (runtime representation of types).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeValue {
    /// The type name as it appears in CEL.
    pub name: Arc<str>,
}

impl TypeValue {
    /// Create a new type value.
    pub fn new(name: impl Into<Arc<str>>) -> Self {
        Self { name: name.into() }
    }

    /// Common type values.
    pub fn null_type() -> Self {
        Self::new("null_type")
    }
    pub fn bool_type() -> Self {
        Self::new("bool")
    }
    pub fn int_type() -> Self {
        Self::new("int")
    }
    pub fn uint_type() -> Self {
        Self::new("uint")
    }
    pub fn double_type() -> Self {
        Self::new("double")
    }
    pub fn string_type() -> Self {
        Self::new("string")
    }
    pub fn bytes_type() -> Self {
        Self::new("bytes")
    }
    pub fn list_type() -> Self {
        Self::new("list")
    }
    pub fn map_type() -> Self {
        Self::new("map")
    }
    pub fn timestamp_type() -> Self {
        Self::new("google.protobuf.Timestamp")
    }
    pub fn duration_type() -> Self {
        Self::new("google.protobuf.Duration")
    }
    pub fn type_type() -> Self {
        Self::new("type")
    }
}

/// A CEL optional value.
#[derive(Debug, Clone)]
pub enum OptionalValue {
    /// An absent optional value.
    None,
    /// A present optional value.
    Some(Box<Value>),
}

impl OptionalValue {
    /// Create an absent optional.
    pub fn none() -> Self {
        OptionalValue::None
    }

    /// Create a present optional.
    pub fn some(value: Value) -> Self {
        OptionalValue::Some(Box::new(value))
    }

    /// Returns true if the optional is present.
    pub fn is_present(&self) -> bool {
        matches!(self, OptionalValue::Some(_))
    }

    /// Get the inner value, or None if absent.
    pub fn as_value(&self) -> Option<&Value> {
        match self {
            OptionalValue::None => None,
            OptionalValue::Some(v) => Some(v),
        }
    }

    /// Unwrap the value or return a default.
    pub fn unwrap_or(self, default: Value) -> Value {
        match self {
            OptionalValue::None => default,
            OptionalValue::Some(v) => *v,
        }
    }
}

/// A CEL map with heterogeneous keys.
///
/// Uses a BTreeMap with a custom key type for deterministic iteration order.
#[derive(Debug, Clone, Default)]
pub struct ValueMap {
    entries: BTreeMap<MapKey, Value>,
}

/// A map key that supports CEL's key types.
///
/// CEL allows bool, int, uint, and string as map keys.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MapKey {
    Bool(bool),
    Int(i64),
    UInt(u64),
    String(Arc<str>),
}

impl MapKey {
    /// Create a map key from a Value.
    pub fn from_value(value: &Value) -> Option<Self> {
        match value {
            Value::Bool(b) => Some(MapKey::Bool(*b)),
            Value::Int(i) => Some(MapKey::Int(*i)),
            Value::UInt(u) => Some(MapKey::UInt(*u)),
            Value::String(s) => Some(MapKey::String(s.clone())),
            _ => None,
        }
    }

    /// Convert back to a Value.
    pub fn to_value(&self) -> Value {
        match self {
            MapKey::Bool(b) => Value::Bool(*b),
            MapKey::Int(i) => Value::Int(*i),
            MapKey::UInt(u) => Value::UInt(*u),
            MapKey::String(s) => Value::String(s.clone()),
        }
    }
}

impl ValueMap {
    /// Create a new empty map.
    pub fn new() -> Self {
        Self {
            entries: BTreeMap::new(),
        }
    }

    /// Create a map from an iterator of key-value pairs.
    pub fn from_entries(entries: impl IntoIterator<Item = (MapKey, Value)>) -> Self {
        Self {
            entries: entries.into_iter().collect(),
        }
    }

    /// Get a value by key.
    pub fn get(&self, key: &MapKey) -> Option<&Value> {
        self.entries.get(key)
    }

    /// Insert a key-value pair.
    pub fn insert(&mut self, key: MapKey, value: Value) {
        self.entries.insert(key, value);
    }

    /// Check if a key exists.
    pub fn contains_key(&self, key: &MapKey) -> bool {
        self.entries.contains_key(key)
    }

    /// Get the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the map is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Iterate over entries.
    pub fn iter(&self) -> impl Iterator<Item = (&MapKey, &Value)> {
        self.entries.iter()
    }

    /// Iterate over keys.
    pub fn keys(&self) -> impl Iterator<Item = &MapKey> {
        self.entries.keys()
    }

    /// Iterate over values.
    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.entries.values()
    }
}

// ==================== Value Constructors ====================

impl Value {
    /// Create a string value.
    pub fn string(s: impl Into<Arc<str>>) -> Self {
        Value::String(s.into())
    }

    /// Create a bytes value.
    pub fn bytes(b: impl Into<Arc<[u8]>>) -> Self {
        Value::Bytes(b.into())
    }

    /// Create a list value.
    pub fn list(elements: impl Into<Arc<[Value]>>) -> Self {
        Value::List(elements.into())
    }

    /// Create a map value.
    pub fn map(entries: impl IntoIterator<Item = (MapKey, Value)>) -> Self {
        Value::Map(Arc::new(ValueMap::from_entries(entries)))
    }

    /// Create a timestamp value.
    pub fn timestamp(seconds: i64, nanos: i32) -> Self {
        Value::Timestamp(Timestamp::new(seconds, nanos))
    }

    /// Create a duration value.
    pub fn duration(seconds: i64, nanos: i32) -> Self {
        Value::Duration(Duration::new(seconds, nanos))
    }

    /// Create a type value.
    pub fn new_type(name: impl Into<Arc<str>>) -> Self {
        Value::Type(TypeValue::new(name))
    }

    /// Create an optional none value.
    pub fn optional_none() -> Self {
        Value::Optional(OptionalValue::None)
    }

    /// Create an optional some value.
    pub fn optional_some(value: Value) -> Self {
        Value::Optional(OptionalValue::some(value))
    }

    /// Create an error value.
    pub fn error(err: impl Into<EvalError>) -> Self {
        Value::Error(Arc::new(err.into()))
    }
}

// ==================== Type Information ====================

impl Value {
    /// Get the CEL type of this value.
    pub fn cel_type(&self) -> CelType {
        match self {
            Value::Null => CelType::Null,
            Value::Bool(_) => CelType::Bool,
            Value::Int(_) => CelType::Int,
            Value::UInt(_) => CelType::UInt,
            Value::Double(_) => CelType::Double,
            Value::String(_) => CelType::String,
            Value::Bytes(_) => CelType::Bytes,
            Value::List(_) => CelType::list(CelType::Dyn),
            Value::Map(_) => CelType::map(CelType::Dyn, CelType::Dyn),
            Value::Timestamp(_) => CelType::Timestamp,
            Value::Duration(_) => CelType::Duration,
            Value::Type(_) => CelType::type_of(CelType::Dyn),
            Value::Optional(opt) => match opt {
                OptionalValue::None => CelType::optional(CelType::Dyn),
                OptionalValue::Some(v) => CelType::optional(v.cel_type()),
            },
            Value::Error(_) => CelType::Error,
        }
    }

    /// Get the CEL type value for this value (for the `type()` function).
    pub fn type_value(&self) -> TypeValue {
        match self {
            Value::Null => TypeValue::null_type(),
            Value::Bool(_) => TypeValue::bool_type(),
            Value::Int(_) => TypeValue::int_type(),
            Value::UInt(_) => TypeValue::uint_type(),
            Value::Double(_) => TypeValue::double_type(),
            Value::String(_) => TypeValue::string_type(),
            Value::Bytes(_) => TypeValue::bytes_type(),
            Value::List(_) => TypeValue::list_type(),
            Value::Map(_) => TypeValue::map_type(),
            Value::Timestamp(_) => TypeValue::timestamp_type(),
            Value::Duration(_) => TypeValue::duration_type(),
            Value::Type(_) => TypeValue::type_type(),
            Value::Optional(_) => TypeValue::new("optional"),
            Value::Error(_) => TypeValue::new("error"),
        }
    }

    /// Check if this value is an error.
    pub fn is_error(&self) -> bool {
        matches!(self, Value::Error(_))
    }

    /// Check if this value is null.
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }

    /// Check if this value is truthy (for bool values).
    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

// ==================== Value Conversions ====================

impl Value {
    /// Try to convert to bool.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Try to convert to i64.
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(i) => Some(*i),
            _ => None,
        }
    }

    /// Try to convert to u64.
    pub fn as_uint(&self) -> Option<u64> {
        match self {
            Value::UInt(u) => Some(*u),
            _ => None,
        }
    }

    /// Try to convert to f64.
    pub fn as_double(&self) -> Option<f64> {
        match self {
            Value::Double(d) => Some(*d),
            _ => None,
        }
    }

    /// Try to convert to string slice.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Try to convert to bytes slice.
    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            Value::Bytes(b) => Some(b),
            _ => None,
        }
    }

    /// Try to convert to list.
    pub fn as_list(&self) -> Option<&[Value]> {
        match self {
            Value::List(l) => Some(l),
            _ => None,
        }
    }

    /// Try to convert to map.
    pub fn as_map(&self) -> Option<&ValueMap> {
        match self {
            Value::Map(m) => Some(m),
            _ => None,
        }
    }

    /// Try to convert to timestamp.
    pub fn as_timestamp(&self) -> Option<Timestamp> {
        match self {
            Value::Timestamp(t) => Some(*t),
            _ => None,
        }
    }

    /// Try to convert to duration.
    pub fn as_duration(&self) -> Option<Duration> {
        match self {
            Value::Duration(d) => Some(*d),
            _ => None,
        }
    }

    /// Try to convert to optional.
    pub fn as_optional(&self) -> Option<&OptionalValue> {
        match self {
            Value::Optional(o) => Some(o),
            _ => None,
        }
    }

    /// Try to get the error.
    pub fn as_error(&self) -> Option<&EvalError> {
        match self {
            Value::Error(e) => Some(e),
            _ => None,
        }
    }
}

// ==================== Equality ====================

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::UInt(a), Value::UInt(b)) => a == b,
            (Value::Double(a), Value::Double(b)) => {
                // CEL follows IEEE 754 semantics: NaN != NaN
                a == b
            }
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                for (key, val_a) in a.iter() {
                    match b.get(key) {
                        Some(val_b) if val_a == val_b => continue,
                        _ => return false,
                    }
                }
                true
            }
            (Value::Timestamp(a), Value::Timestamp(b)) => a == b,
            (Value::Duration(a), Value::Duration(b)) => a == b,
            (Value::Type(a), Value::Type(b)) => a == b,
            (Value::Optional(a), Value::Optional(b)) => match (a, b) {
                (OptionalValue::None, OptionalValue::None) => true,
                (OptionalValue::Some(va), OptionalValue::Some(vb)) => va == vb,
                _ => false,
            },
            _ => false,
        }
    }
}

// ==================== Comparison ====================

impl Value {
    /// Compare two values, returning an ordering if comparable.
    ///
    /// CEL supports comparison between values of the same type,
    /// and between numeric types (int, uint, double).
    pub fn compare(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Some(a.cmp(b)),
            (Value::Int(a), Value::Int(b)) => Some(a.cmp(b)),
            (Value::UInt(a), Value::UInt(b)) => Some(a.cmp(b)),
            (Value::Double(a), Value::Double(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => Some(a.cmp(b)),
            (Value::Bytes(a), Value::Bytes(b)) => Some(a.cmp(b)),
            (Value::Timestamp(a), Value::Timestamp(b)) => {
                Some((a.seconds, a.nanos).cmp(&(b.seconds, b.nanos)))
            }
            (Value::Duration(a), Value::Duration(b)) => {
                Some((a.seconds, a.nanos).cmp(&(b.seconds, b.nanos)))
            }
            // Cross-numeric comparisons
            (Value::Int(a), Value::UInt(b)) => {
                if *a < 0 {
                    Some(Ordering::Less)
                } else {
                    (*a as u64).partial_cmp(b)
                }
            }
            (Value::UInt(a), Value::Int(b)) => {
                if *b < 0 {
                    Some(Ordering::Greater)
                } else {
                    a.partial_cmp(&(*b as u64))
                }
            }
            (Value::Int(a), Value::Double(b)) => (*a as f64).partial_cmp(b),
            (Value::Double(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Value::UInt(a), Value::Double(b)) => (*a as f64).partial_cmp(b),
            (Value::Double(a), Value::UInt(b)) => a.partial_cmp(&(*b as f64)),
            _ => None,
        }
    }
}

// ==================== Display ====================

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::UInt(v) => write!(f, "{}u", v),
            Value::Double(v) => {
                if v.is_nan() {
                    write!(f, "NaN")
                } else if v.is_infinite() {
                    if v.is_sign_positive() {
                        write!(f, "+infinity")
                    } else {
                        write!(f, "-infinity")
                    }
                } else if v.fract() == 0.0 {
                    write!(f, "{}.0", v)
                } else {
                    write!(f, "{}", v)
                }
            }
            Value::String(v) => write!(f, "\"{}\"", v),
            Value::Bytes(v) => write!(f, "b\"{}\"", String::from_utf8_lossy(v)),
            Value::List(v) => {
                write!(f, "[")?;
                for (i, elem) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Value::Map(m) => {
                write!(f, "{{")?;
                for (i, (key, value)) in m.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key.to_value(), value)?;
                }
                write!(f, "}}")
            }
            Value::Timestamp(t) => write!(f, "timestamp({})", t.seconds),
            Value::Duration(d) => write!(f, "duration({}s)", d.seconds),
            Value::Type(t) => write!(f, "type({})", t.name),
            Value::Optional(o) => match o {
                OptionalValue::None => write!(f, "optional.none()"),
                OptionalValue::Some(v) => write!(f, "optional.of({})", v),
            },
            Value::Error(e) => write!(f, "error({})", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_equality() {
        assert_eq!(Value::Int(42), Value::Int(42));
        assert_ne!(Value::Int(42), Value::Int(43));
        assert_ne!(Value::Int(42), Value::UInt(42));
        assert_eq!(Value::string("hello"), Value::string("hello"));
    }

    #[test]
    fn test_value_comparison() {
        assert_eq!(Value::Int(1).compare(&Value::Int(2)), Some(Ordering::Less));
        assert_eq!(
            Value::Int(2).compare(&Value::Int(1)),
            Some(Ordering::Greater)
        );
        assert_eq!(Value::Int(1).compare(&Value::Int(1)), Some(Ordering::Equal));

        // Cross-numeric comparison
        assert_eq!(Value::Int(-1).compare(&Value::UInt(1)), Some(Ordering::Less));
        assert_eq!(
            Value::Int(1).compare(&Value::Double(1.5)),
            Some(Ordering::Less)
        );
    }

    #[test]
    fn test_map_operations() {
        let mut map = ValueMap::new();
        map.insert(MapKey::String(Arc::from("key")), Value::Int(42));

        assert_eq!(map.len(), 1);
        assert_eq!(
            map.get(&MapKey::String(Arc::from("key"))),
            Some(&Value::Int(42))
        );
        assert!(map.contains_key(&MapKey::String(Arc::from("key"))));
        assert!(!map.contains_key(&MapKey::String(Arc::from("other"))));
    }

    #[test]
    fn test_optional_value() {
        let none = OptionalValue::none();
        assert!(!none.is_present());
        assert!(none.as_value().is_none());

        let some = OptionalValue::some(Value::Int(42));
        assert!(some.is_present());
        assert_eq!(some.as_value(), Some(&Value::Int(42)));
    }

    #[test]
    fn test_timestamp_comparison() {
        let t1 = Timestamp::new(100, 0);
        let t2 = Timestamp::new(200, 0);
        let t3 = Timestamp::new(100, 500);

        assert!(t1.is_before(&t2));
        assert!(t2.is_after(&t1));
        assert!(t1.is_before(&t3));
    }

    #[test]
    fn test_duration_nanos() {
        let d = Duration::from_nanos(1_500_000_000);
        assert_eq!(d.seconds, 1);
        assert_eq!(d.nanos, 500_000_000);
        assert_eq!(d.to_nanos(), 1_500_000_000);
    }

    #[test]
    fn test_cel_type() {
        assert_eq!(Value::Int(42).cel_type(), CelType::Int);
        assert_eq!(Value::string("hello").cel_type(), CelType::String);
        assert_eq!(
            Value::list(vec![Value::Int(1)]).cel_type(),
            CelType::list(CelType::Dyn)
        );
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Value::Null), "null");
        assert_eq!(format!("{}", Value::Int(42)), "42");
        assert_eq!(format!("{}", Value::UInt(42)), "42u");
        assert_eq!(format!("{}", Value::string("hello")), "\"hello\"");
        assert_eq!(format!("{}", Value::Bool(true)), "true");
    }
}
