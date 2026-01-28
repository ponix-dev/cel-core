//! Abbreviations let you use short names for fully-qualified protobuf types.
//!
//! Run with: cargo run -p cel-core --example abbreviations

use cel_core::eval::{Duration, MapActivation, Timestamp, Value};
use cel_core::types::ProtoTypeRegistry;
use cel_core::{Abbreviations, CelType, Env};

fn main() {
    let registry = ProtoTypeRegistry::new();

    let abbrevs = Abbreviations::new()
        .add("google.protobuf.Timestamp")
        .unwrap()
        .add("google.protobuf.Duration")
        .unwrap();

    let env = Env::with_standard_library()
        .with_proto_types(registry)
        .with_abbreviations(abbrevs)
        .with_variable("event_time", CelType::Timestamp)
        .with_variable("timeout", CelType::Duration);

    // Construct proto messages using abbreviated names
    let ast = env.compile("Timestamp{seconds: 1704067200}").unwrap();
    let program = env.program(&ast).unwrap();
    let result = program.eval(&MapActivation::new());

    println!("Abbreviations: Timestamp -> google.protobuf.Timestamp");
    println!("               Duration  -> google.protobuf.Duration\n");
    println!("Expression: Timestamp{{seconds: 1704067200}}");
    println!("Result: {}\n", result);

    // Calculate deadline: event_time + timeout
    let ast = env.compile("event_time + timeout").unwrap();
    let program = env.program(&ast).unwrap();

    let mut activation = MapActivation::new();
    activation.insert("event_time", Value::Timestamp(Timestamp::from_seconds(1704110400)));
    activation.insert("timeout", Value::Duration(Duration::from_seconds(1800)));

    let result = program.eval(&activation);

    println!("Expression: event_time + timeout");
    println!("  event_time = 2024-01-01 12:00:00 UTC");
    println!("  timeout = 30 minutes");
    println!("Result: {}", result);
}
