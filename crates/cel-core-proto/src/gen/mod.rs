//! Generated protobuf types from cel-spec.

// Re-export prost_types for the generated code
pub use prost_types;

pub mod cel {
    pub mod expr {
        include!("cel/expr/cel.expr.rs");

        pub mod conformance {
            include!("cel/expr/conformance/cel.expr.conformance.rs");

            pub mod test {
                include!("cel/expr/conformance/test/cel.expr.conformance.test.rs");
            }

            pub mod proto2 {
                include!("cel/expr/conformance/proto2/cel.expr.conformance.proto2.rs");
            }

            pub mod proto3 {
                include!("cel/expr/conformance/proto3/cel.expr.conformance.proto3.rs");
            }
        }
    }

    pub mod policy {
        include!("cel/policy/cel.policy.rs");
    }
}
