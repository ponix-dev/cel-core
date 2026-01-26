//! Bidirectional operator mapping between cel-core types and proto function names.

use cel_core::types::{BinaryOp, UnaryOp};

/// Convert a unary operator to its proto function name.
pub fn unary_op_to_function(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "_-_",
        UnaryOp::Not => "!_",
    }
}

/// Convert a binary operator to its proto function name.
pub fn binary_op_to_function(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "_+_",
        BinaryOp::Sub => "_-_",
        BinaryOp::Mul => "_*_",
        BinaryOp::Div => "_/_",
        BinaryOp::Mod => "_%_",
        BinaryOp::Eq => "_==_",
        BinaryOp::Ne => "_!=_",
        BinaryOp::Lt => "_<_",
        BinaryOp::Le => "_<=_",
        BinaryOp::Gt => "_>_",
        BinaryOp::Ge => "_>=_",
        BinaryOp::In => "@in",
        BinaryOp::And => "_&&_",
        BinaryOp::Or => "_||_",
    }
}

/// Try to convert a proto function name to a unary operator.
pub fn function_to_unary_op(function: &str) -> Option<UnaryOp> {
    match function {
        "_-_" => Some(UnaryOp::Neg),
        "!_" => Some(UnaryOp::Not),
        _ => None,
    }
}

/// Try to convert a proto function name to a binary operator.
pub fn function_to_binary_op(function: &str) -> Option<BinaryOp> {
    match function {
        "_+_" => Some(BinaryOp::Add),
        "_-_" => Some(BinaryOp::Sub),
        "_*_" => Some(BinaryOp::Mul),
        "_/_" => Some(BinaryOp::Div),
        "_%_" => Some(BinaryOp::Mod),
        "_==_" => Some(BinaryOp::Eq),
        "_!=_" => Some(BinaryOp::Ne),
        "_<_" => Some(BinaryOp::Lt),
        "_<=_" => Some(BinaryOp::Le),
        "_>_" => Some(BinaryOp::Gt),
        "_>=_" => Some(BinaryOp::Ge),
        "@in" => Some(BinaryOp::In),
        "_&&_" => Some(BinaryOp::And),
        "_||_" => Some(BinaryOp::Or),
        _ => None,
    }
}

/// Check if a function name represents the ternary conditional operator.
pub fn is_ternary_function(function: &str) -> bool {
    function == "_?_:_"
}

/// Check if a function name represents the index operator.
pub fn is_index_function(function: &str) -> bool {
    function == "_[_]"
}

/// The proto function name for the ternary conditional operator.
pub const TERNARY_FUNCTION: &str = "_?_:_";

/// The proto function name for the index operator.
pub const INDEX_FUNCTION: &str = "_[_]";

/// The proto function name for the optional index operator.
pub const OPTIONAL_INDEX_FUNCTION: &str = "_[?_]";

/// The proto function name for the optional select operator.
pub const OPTIONAL_SELECT_FUNCTION: &str = "_?._";

/// Check if a function name represents the optional index operator.
pub fn is_optional_index_function(function: &str) -> bool {
    function == "_[?_]"
}

/// Check if a function name represents the optional select operator.
pub fn is_optional_select_function(function: &str) -> bool {
    function == "_?._"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unary_roundtrip() {
        for op in [UnaryOp::Neg, UnaryOp::Not] {
            let func = unary_op_to_function(op);
            assert_eq!(function_to_unary_op(func), Some(op));
        }
    }

    #[test]
    fn test_binary_roundtrip() {
        let ops = [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Mod,
            BinaryOp::Eq,
            BinaryOp::Ne,
            BinaryOp::Lt,
            BinaryOp::Le,
            BinaryOp::Gt,
            BinaryOp::Ge,
            BinaryOp::In,
            BinaryOp::And,
            BinaryOp::Or,
        ];
        for op in ops {
            let func = binary_op_to_function(op);
            assert_eq!(function_to_binary_op(func), Some(op));
        }
    }

    #[test]
    fn test_special_functions() {
        assert!(is_ternary_function("_?_:_"));
        assert!(!is_ternary_function("_+_"));
        assert!(is_index_function("_[_]"));
        assert!(!is_index_function("_+_"));
    }

    #[test]
    fn test_neg_sub_ambiguity() {
        // Both UnaryOp::Neg and BinaryOp::Sub map to "_-_"
        // When converting back, we need context (arg count) to distinguish
        assert_eq!(unary_op_to_function(UnaryOp::Neg), "_-_");
        assert_eq!(binary_op_to_function(BinaryOp::Sub), "_-_");
        // function_to_unary_op returns Some for "_-_" (valid unary)
        assert_eq!(function_to_unary_op("_-_"), Some(UnaryOp::Neg));
        // function_to_binary_op also returns Some for "_-_" (valid binary)
        assert_eq!(function_to_binary_op("_-_"), Some(BinaryOp::Sub));
    }
}
