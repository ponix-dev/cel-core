//! Legacy enum mode: enum values are returned as plain integers.
//!
//! Run with: cargo run -p cel-core --example enums_legacy

use cel_core::eval::MapActivation;
use cel_core::types::ProtoTypeRegistry;
use cel_core::Env;

fn main() {
    // Build a proto registry with the conformance test descriptors (which contain enums)
    let mut registry = ProtoTypeRegistry::new();
    registry
        .add_file_descriptor_set(cel_core_proto::gen::cel::expr::conformance::proto3::FILE_DESCRIPTOR_SET)
        .expect("Failed to add proto3 descriptors");

    // Legacy mode: enums behave as plain integers
    let env = Env::with_standard_library()
        .with_proto_types(registry)
        .with_container("cel.expr.conformance.proto3")
        .with_legacy_enums();

    let activation = MapActivation::new();

    println!("=== Legacy Enum Mode ===\n");

    let cases: &[(&str, &str)] = &[
        // Values — enums resolve to plain ints
        ("GlobalEnum.GAZ", "Enum constant as int"),
        ("TestAllTypes.NestedEnum.BAZ", "Nested enum constant as int"),
        ("type(GlobalEnum.GAZ)", "type() returns int"),

        // Comparisons — since enums are ints, int comparisons work directly
        ("GlobalEnum.GAR == 1", "Enum == int (same value -> true)"),
        ("GlobalEnum.GAR == 2", "Enum == int (different value -> false)"),
        ("GlobalEnum.GAZ == TestAllTypes.NestedEnum.BAZ", "Two enums with same int value (both 2 -> true)"),
        ("GlobalEnum.GOO == TestAllTypes.NestedEnum.FOO", "Two enums with same int value (both 0 -> true)"),

        // Arithmetic — works naturally since enums are just ints
        ("TestAllTypes.NestedEnum.BAR + 1", "Enum + int arithmetic"),
    ];

    for (expr, description) in cases {
        let ast = env.compile(expr).unwrap();
        let program = env.program(&ast).unwrap();
        let result = program.eval(&activation);
        println!("{:>55} | {} = {}", description, expr, result);
    }
}
