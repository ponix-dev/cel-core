//! Containers set your default protobuf package for type resolution.
//!
//! Run with: cargo run -p cel-core --example containers

use cel_core::eval::MapActivation;
use cel_core::types::ProtoTypeRegistry;
use cel_core::Env;

fn main() {
    let registry = ProtoTypeRegistry::new();

    // With container "google.protobuf", we can use short type names
    let env = Env::with_standard_library()
        .with_proto_types(registry)
        .with_container("google.protobuf");

    // Construct a Timestamp using short name (instead of google.protobuf.Timestamp)
    let ast = env.compile("Timestamp{seconds: 1704067200}").unwrap();
    let program = env.program(&ast).unwrap();
    let result = program.eval(&MapActivation::new());

    println!("Container: google.protobuf\n");
    println!("Expression: Timestamp{{seconds: 1704067200}}");
    println!("Result: {}\n", result);

    // Construct a Duration
    let ast = env.compile("Duration{seconds: 3600}").unwrap();
    let program = env.program(&ast).unwrap();
    let result = program.eval(&MapActivation::new());

    println!("Expression: Duration{{seconds: 3600}}");
    println!("Result: {}\n", result);

    // Without container, would need: google.protobuf.Timestamp{seconds: ...}
    println!("Without container, you'd write:");
    println!("  google.protobuf.Timestamp{{seconds: 1704067200}}");
    println!("  google.protobuf.Duration{{seconds: 3600}}");
}
