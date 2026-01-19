// @generated
pub mod cel {
    // @@protoc_insertion_point(attribute:cel.expr)
    pub mod expr {
        include!("cel/expr/cel.expr.rs");
        // @@protoc_insertion_point(cel.expr)
        // @@protoc_insertion_point(attribute:cel.expr.conformance)
        pub mod conformance {
            include!("cel/expr/conformance/cel.expr.conformance.rs");
            // @@protoc_insertion_point(cel.expr.conformance)
            // @@protoc_insertion_point(attribute:cel.expr.conformance.proto2)
            pub mod proto2 {
                include!("cel/expr/conformance/proto2/cel.expr.conformance.proto2.rs");
                // @@protoc_insertion_point(cel.expr.conformance.proto2)
            }
            // @@protoc_insertion_point(attribute:cel.expr.conformance.proto3)
            pub mod proto3 {
                include!("cel/expr/conformance/proto3/cel.expr.conformance.proto3.rs");
                // @@protoc_insertion_point(cel.expr.conformance.proto3)
            }
            // @@protoc_insertion_point(attribute:cel.expr.conformance.test)
            pub mod test {
                include!("cel/expr/conformance/test/cel.expr.conformance.test.rs");
                // @@protoc_insertion_point(cel.expr.conformance.test)
            }
        }
    }
    // @@protoc_insertion_point(attribute:cel.policy)
    pub mod policy {
        include!("cel/policy/cel.policy.rs");
        // @@protoc_insertion_point(cel.policy)
    }
}
