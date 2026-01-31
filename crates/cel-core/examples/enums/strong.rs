//! Strong enum mode (default): enum values carry type information.
//!
//! Run with: cargo run -p cel-core --example enums_strong

use cel_core::eval::MapActivation;
use cel_core::types::ProtoTypeRegistry;
use cel_core::Env;

fn main() {
    // Build a proto registry with the conformance test descriptors (which contain enums)
    let mut registry = ProtoTypeRegistry::new();
    registry
        .add_file_descriptor_set(cel_core_proto::gen::cel::expr::conformance::proto3::FILE_DESCRIPTOR_SET)
        .expect("Failed to add proto3 descriptors");

    // Strong mode is the default — enums carry their fully-qualified type name
    let env = Env::with_standard_library()
        .with_proto_types(registry)
        .with_container("cel.expr.conformance.proto3");

    let activation = MapActivation::new();

    println!("=== Strong Enum Mode (Default) ===\n");

    let cases: &[(&str, &str)] = &[
        // Values — enums resolve to typed EnumValue
        ("GlobalEnum.GAZ", "Enum constant with type info"),
        ("TestAllTypes.NestedEnum.BAR", "Nested enum constant"),
        ("type(GlobalEnum.GAZ)", "type() returns enum type name"),
        ("type(TestAllTypes.NestedEnum.BAR)", "Nested enum type name"),

        // Constructors — create enums from int or string
        ("TestAllTypes.NestedEnum(1)", "Construct from int"),
        ("TestAllTypes.NestedEnum('BAZ')", "Construct from string"),

        // Comparisons — same enum type and value
        ("GlobalEnum.GAZ == GlobalEnum.GAZ", "Same enum, same value (true)"),
        ("GlobalEnum.GAZ == GlobalEnum.GAR", "Same enum, different value (false)"),
        ("GlobalEnum.GAZ == GlobalEnum(2)", "Constant vs constructor (true)"),
        ("TestAllTypes.NestedEnum.BAR == TestAllTypes.NestedEnum(1)", "Nested: constant vs constructor (true)"),

        // Different enum types with same numeric value are NOT equal
        ("GlobalEnum.GAZ == TestAllTypes.NestedEnum.BAZ", "Different enum types, same int (false)"),

        // Convert to int for numeric comparison
        ("int(GlobalEnum.GAZ)", "int() extracts numeric value"),
        ("int(GlobalEnum.GAZ) == 2", "int(enum) == int (true)"),
        ("int(GlobalEnum.GAZ) == int(TestAllTypes.NestedEnum.BAZ)", "Compare via int() (true)"),
    ];

    for (expr, description) in cases {
        let ast = env.compile(expr).unwrap();
        let program = env.program(&ast).unwrap();
        let result = program.eval(&activation);
        println!("{:>55} | {} = {}", description, expr, result);
    }
}
