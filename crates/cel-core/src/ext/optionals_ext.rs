//! Optionals extension library for CEL.
//!
//! This module provides optional type support for CEL,
//! matching the cel-go optionals extension.
//!
//! # Functions
//!
//! - `optional.of(T) -> optional<T>` - Wrap a value in an optional
//! - `optional.none() -> optional<dyn>` - Create an empty optional
//! - `optional.ofNonZeroValue(T) -> optional<T>` - Wrap if non-zero
//!
//! # Methods
//!
//! - `.hasValue() -> bool` - Check if optional has a value
//! - `.value() -> T` - Get the value (errors if absent)
//! - `.or(optional<T>) -> optional<T>` - Return first present optional
//! - `.orValue(T) -> T` - Return value or default

use crate::types::{CelType, FunctionDecl, OverloadDecl};

/// Returns the optionals extension library function declarations.
pub fn optionals_extension() -> Vec<FunctionDecl> {
    vec![
        // ===== Global functions =====
        // optional.of(T) -> optional<T>
        FunctionDecl::new("optional.of").with_overload(
            OverloadDecl::function(
                "optional_of",
                vec![CelType::type_param("T")],
                CelType::optional(CelType::type_param("T")),
            )
            .with_type_params(vec!["T".to_string()]),
        ),
        // optional.none() -> optional<dyn>
        FunctionDecl::new("optional.none").with_overload(OverloadDecl::function(
            "optional_none",
            vec![],
            CelType::optional(CelType::Dyn),
        )),
        // optional.ofNonZeroValue(T) -> optional<T>
        FunctionDecl::new("optional.ofNonZeroValue").with_overload(
            OverloadDecl::function(
                "optional_of_non_zero_value",
                vec![CelType::type_param("T")],
                CelType::optional(CelType::type_param("T")),
            )
            .with_type_params(vec!["T".to_string()]),
        ),
        // ===== Methods on optional<T> =====
        // .hasValue() -> bool
        FunctionDecl::new("hasValue").with_overload(
            OverloadDecl::method(
                "optional_has_value",
                vec![CelType::optional(CelType::type_param("T"))],
                CelType::Bool,
            )
            .with_type_params(vec!["T".to_string()]),
        ),
        // .value() -> T
        FunctionDecl::new("value").with_overload(
            OverloadDecl::method(
                "optional_value",
                vec![CelType::optional(CelType::type_param("T"))],
                CelType::type_param("T"),
            )
            .with_type_params(vec!["T".to_string()]),
        ),
        // .or(optional<T>) -> optional<T>
        FunctionDecl::new("or").with_overload(
            OverloadDecl::method(
                "optional_or_optional",
                vec![
                    CelType::optional(CelType::type_param("T")),
                    CelType::optional(CelType::type_param("T")),
                ],
                CelType::optional(CelType::type_param("T")),
            )
            .with_type_params(vec!["T".to_string()]),
        ),
        // .orValue(T) -> T
        FunctionDecl::new("orValue").with_overload(
            OverloadDecl::method(
                "optional_or_value",
                vec![
                    CelType::optional(CelType::type_param("T")),
                    CelType::type_param("T"),
                ],
                CelType::type_param("T"),
            )
            .with_type_params(vec!["T".to_string()]),
        ),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_optionals_extension_has_functions() {
        let funcs = optionals_extension();
        // optional.of, optional.none, optional.ofNonZeroValue
        // hasValue, value, or, orValue
        assert_eq!(funcs.len(), 7);
    }

    #[test]
    fn test_optional_of() {
        let funcs = optionals_extension();
        let of_func = funcs.iter().find(|f| f.name == "optional.of").unwrap();
        assert_eq!(of_func.overloads.len(), 1);
        assert_eq!(of_func.overloads[0].id, "optional_of");
        assert!(!of_func.overloads[0].is_member);
    }

    #[test]
    fn test_optional_none() {
        let funcs = optionals_extension();
        let none_func = funcs.iter().find(|f| f.name == "optional.none").unwrap();
        assert_eq!(none_func.overloads.len(), 1);
        assert_eq!(none_func.overloads[0].id, "optional_none");
        assert!(none_func.overloads[0].params.is_empty());
    }

    #[test]
    fn test_has_value() {
        let funcs = optionals_extension();
        let has_value = funcs.iter().find(|f| f.name == "hasValue").unwrap();
        assert_eq!(has_value.overloads.len(), 1);
        assert!(has_value.overloads[0].is_member);
        assert_eq!(has_value.overloads[0].result, CelType::Bool);
    }

    #[test]
    fn test_value() {
        let funcs = optionals_extension();
        let value_func = funcs.iter().find(|f| f.name == "value").unwrap();
        assert_eq!(value_func.overloads.len(), 1);
        assert!(value_func.overloads[0].is_member);
    }

    #[test]
    fn test_or() {
        let funcs = optionals_extension();
        let or_func = funcs.iter().find(|f| f.name == "or").unwrap();
        assert_eq!(or_func.overloads.len(), 1);
        assert!(or_func.overloads[0].is_member);
        // Takes optional<T> and returns optional<T>
        assert_eq!(or_func.overloads[0].params.len(), 2);
    }

    #[test]
    fn test_or_value() {
        let funcs = optionals_extension();
        let or_value = funcs.iter().find(|f| f.name == "orValue").unwrap();
        assert_eq!(or_value.overloads.len(), 1);
        assert!(or_value.overloads[0].is_member);
        // Takes optional<T>, T and returns T
        assert_eq!(or_value.overloads[0].params.len(), 2);
    }

    #[test]
    fn test_all_global_functions_are_standalone() {
        let funcs = optionals_extension();
        let global_funcs = ["optional.of", "optional.none", "optional.ofNonZeroValue"];
        for name in global_funcs {
            let func = funcs.iter().find(|f| f.name == name).unwrap();
            for overload in &func.overloads {
                assert!(
                    !overload.is_member,
                    "Expected {} to be standalone, but it's a member function",
                    overload.id
                );
            }
        }
    }

    #[test]
    fn test_all_methods_are_members() {
        let funcs = optionals_extension();
        let methods = ["hasValue", "value", "or", "orValue"];
        for name in methods {
            let func = funcs.iter().find(|f| f.name == name).unwrap();
            for overload in &func.overloads {
                assert!(
                    overload.is_member,
                    "Expected {} to be a member function, but it's standalone",
                    overload.id
                );
            }
        }
    }
}
