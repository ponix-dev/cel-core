//! Math extension library for CEL.
//!
//! This module provides additional math functions beyond the CEL standard library,
//! matching the cel-go math extension.
//!
//! # Functions
//!
//! - `math.greatest(...)` - Returns greatest of arguments (variadic or list)
//! - `math.least(...)` - Returns least of arguments (variadic or list)
//! - `math.ceil(double)` - Ceiling function
//! - `math.floor(double)` - Floor function
//! - `math.round(double)` - Round to nearest integer
//! - `math.trunc(double)` - Truncate toward zero
//! - `math.abs(number)` - Absolute value
//! - `math.sign(number)` - Sign of number (-1, 0, or 1)
//! - `math.isNaN(double)` - Check if NaN
//! - `math.isInf(double)` - Check if infinite
//! - `math.isFinite(double)` - Check if finite
//! - `math.bitAnd(int, int)` - Bitwise AND
//! - `math.bitOr(int, int)` - Bitwise OR
//! - `math.bitXor(int, int)` - Bitwise XOR
//! - `math.bitNot(int)` - Bitwise NOT
//! - `math.bitShiftLeft(int, int)` - Left shift
//! - `math.bitShiftRight(int, int)` - Right shift

use crate::types::{CelType, FunctionDecl, OverloadDecl};

/// Returns the math extension library function declarations.
pub fn math_extension() -> Vec<FunctionDecl> {
    let mut funcs = Vec::new();

    // math.greatest and math.least with multiple arities
    funcs.push(build_minmax_function("math.greatest"));
    funcs.push(build_minmax_function("math.least"));

    // Simple double functions
    for name in ["math.ceil", "math.floor", "math.round", "math.trunc"] {
        funcs.push(FunctionDecl::new(name).with_overload(OverloadDecl::function(
            &format!("{}_double", name.replace('.', "_")),
            vec![CelType::Double],
            CelType::Double,
        )));
    }

    // math.abs
    funcs.push(
        FunctionDecl::new("math.abs")
            .with_overload(OverloadDecl::function(
                "math_abs_int",
                vec![CelType::Int],
                CelType::Int,
            ))
            .with_overload(OverloadDecl::function(
                "math_abs_uint",
                vec![CelType::UInt],
                CelType::UInt,
            ))
            .with_overload(OverloadDecl::function(
                "math_abs_double",
                vec![CelType::Double],
                CelType::Double,
            )),
    );

    // math.sign
    funcs.push(
        FunctionDecl::new("math.sign")
            .with_overload(OverloadDecl::function(
                "math_sign_int",
                vec![CelType::Int],
                CelType::Int,
            ))
            .with_overload(OverloadDecl::function(
                "math_sign_uint",
                vec![CelType::UInt],
                CelType::UInt,
            ))
            .with_overload(OverloadDecl::function(
                "math_sign_double",
                vec![CelType::Double],
                CelType::Double,
            )),
    );

    // math.isNaN, isInf, isFinite
    funcs.push(FunctionDecl::new("math.isNaN").with_overload(OverloadDecl::function(
        "math_isnan_double",
        vec![CelType::Double],
        CelType::Bool,
    )));
    funcs.push(FunctionDecl::new("math.isInf").with_overload(OverloadDecl::function(
        "math_isinf_double",
        vec![CelType::Double],
        CelType::Bool,
    )));
    funcs.push(FunctionDecl::new("math.isFinite").with_overload(OverloadDecl::function(
        "math_isfinite_double",
        vec![CelType::Double],
        CelType::Bool,
    )));

    // Bit operations
    add_bit_operations(&mut funcs);

    funcs
}

fn build_minmax_function(name: &str) -> FunctionDecl {
    let base = name.replace('.', "_");
    let mut decl = FunctionDecl::new(name);

    // Unary (identity)
    for (suffix, cel_type) in [
        ("int", CelType::Int),
        ("uint", CelType::UInt),
        ("double", CelType::Double),
    ] {
        decl = decl.with_overload(OverloadDecl::function(
            &format!("{}_{}", base, suffix),
            vec![cel_type.clone()],
            cel_type,
        ));
    }

    // Binary same-type
    for (suffix, cel_type) in [
        ("int", CelType::Int),
        ("uint", CelType::UInt),
        ("double", CelType::Double),
    ] {
        decl = decl.with_overload(OverloadDecl::function(
            &format!("{}_{}_{}", base, suffix, suffix),
            vec![cel_type.clone(), cel_type.clone()],
            cel_type,
        ));
    }

    // Binary mixed-type -> Dyn
    let types = [
        ("int", CelType::Int),
        ("uint", CelType::UInt),
        ("double", CelType::Double),
    ];
    for (name1, type1) in &types {
        for (name2, type2) in &types {
            if name1 != name2 {
                decl = decl.with_overload(OverloadDecl::function(
                    &format!("{}_{}_{}", base, name1, name2),
                    vec![type1.clone(), type2.clone()],
                    CelType::Dyn,
                ));
            }
        }
    }

    // Ternary and higher arities (3-6 args) for common use cases
    for arity in 3..=6 {
        // All same type
        for (suffix, cel_type) in [
            ("int", CelType::Int),
            ("uint", CelType::UInt),
            ("double", CelType::Double),
        ] {
            decl = decl.with_overload(OverloadDecl::function(
                &format!("{}_{}{}", base, suffix, arity),
                vec![cel_type.clone(); arity],
                cel_type,
            ));
        }
        // Mixed -> Dyn (just one overload for mixed types)
        decl = decl.with_overload(OverloadDecl::function(
            &format!("{}_dyn{}", base, arity),
            vec![CelType::Dyn; arity],
            CelType::Dyn,
        ));
    }

    // List overloads
    for (suffix, cel_type) in [
        ("int", CelType::Int),
        ("uint", CelType::UInt),
        ("double", CelType::Double),
    ] {
        decl = decl.with_overload(OverloadDecl::function(
            &format!("{}_list_{}", base, suffix),
            vec![CelType::list(cel_type.clone())],
            cel_type,
        ));
    }
    decl = decl.with_overload(OverloadDecl::function(
        &format!("{}_list_dyn", base),
        vec![CelType::list(CelType::Dyn)],
        CelType::Dyn,
    ));

    decl
}

fn add_bit_operations(funcs: &mut Vec<FunctionDecl>) {
    // math.bitAnd, bitOr, bitXor (binary)
    for op in ["bitAnd", "bitOr", "bitXor"] {
        funcs.push(
            FunctionDecl::new(&format!("math.{}", op))
                .with_overload(OverloadDecl::function(
                    &format!("math_{}_int_int", op.to_lowercase()),
                    vec![CelType::Int, CelType::Int],
                    CelType::Int,
                ))
                .with_overload(OverloadDecl::function(
                    &format!("math_{}_uint_uint", op.to_lowercase()),
                    vec![CelType::UInt, CelType::UInt],
                    CelType::UInt,
                )),
        );
    }

    // math.bitNot (unary)
    funcs.push(
        FunctionDecl::new("math.bitNot")
            .with_overload(OverloadDecl::function(
                "math_bitnot_int",
                vec![CelType::Int],
                CelType::Int,
            ))
            .with_overload(OverloadDecl::function(
                "math_bitnot_uint",
                vec![CelType::UInt],
                CelType::UInt,
            )),
    );

    // math.bitShiftLeft, bitShiftRight
    for op in ["bitShiftLeft", "bitShiftRight"] {
        funcs.push(
            FunctionDecl::new(&format!("math.{}", op))
                .with_overload(OverloadDecl::function(
                    &format!("math_{}_int_int", op.to_lowercase()),
                    vec![CelType::Int, CelType::Int],
                    CelType::Int,
                ))
                .with_overload(OverloadDecl::function(
                    &format!("math_{}_uint_int", op.to_lowercase()),
                    vec![CelType::UInt, CelType::Int],
                    CelType::UInt,
                )),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_math_extension_has_functions() {
        let funcs = math_extension();
        // Should have: greatest, least, ceil, floor, round, trunc, abs, sign,
        // isNaN, isInf, isFinite, bitAnd, bitOr, bitXor, bitNot, bitShiftLeft, bitShiftRight
        assert!(funcs.len() >= 17);
    }

    #[test]
    fn test_math_greatest_overloads() {
        let funcs = math_extension();
        let greatest = funcs.iter().find(|f| f.name == "math.greatest").unwrap();

        // Check unary overloads exist
        assert!(greatest
            .overloads
            .iter()
            .any(|o| o.id == "math_greatest_int" && o.params.len() == 1));
        assert!(greatest
            .overloads
            .iter()
            .any(|o| o.id == "math_greatest_double" && o.params.len() == 1));

        // Check binary same-type overloads
        assert!(greatest
            .overloads
            .iter()
            .any(|o| o.id == "math_greatest_int_int" && o.params.len() == 2));

        // Check ternary overloads
        assert!(greatest
            .overloads
            .iter()
            .any(|o| o.id == "math_greatest_int3" && o.params.len() == 3));

        // Check list overloads
        assert!(greatest
            .overloads
            .iter()
            .any(|o| o.id == "math_greatest_list_int"));
    }

    #[test]
    fn test_math_abs_overloads() {
        let funcs = math_extension();
        let abs = funcs.iter().find(|f| f.name == "math.abs").unwrap();
        assert_eq!(abs.overloads.len(), 3); // int, uint, double
    }

    #[test]
    fn test_bit_operations() {
        let funcs = math_extension();

        let bit_and = funcs.iter().find(|f| f.name == "math.bitAnd").unwrap();
        assert_eq!(bit_and.overloads.len(), 2); // int, uint

        let bit_not = funcs.iter().find(|f| f.name == "math.bitNot").unwrap();
        assert_eq!(bit_not.overloads.len(), 2); // int, uint
    }

    #[test]
    fn test_all_functions_are_standalone() {
        let funcs = math_extension();
        for func in &funcs {
            for overload in &func.overloads {
                assert!(
                    !overload.is_member,
                    "Expected {} to be standalone, but it's a member function",
                    overload.id
                );
            }
        }
    }
}
