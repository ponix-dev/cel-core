//! Abbreviations let you use short names for fully-qualified protobuf types.
//!
//! Run with: cargo run -p cel-core --example abbreviations

use std::sync::Arc;

use cel_core::{Abbreviations, CelType, Duration, Env, MapActivation, Timestamp, Value};
use cel_core_proto::ProstProtoRegistry;

fn main() {
    let registry = ProstProtoRegistry::new();

    let abbrevs = Abbreviations::new()
        .with_abbreviation("google.protobuf.Timestamp")
        .unwrap()
        .with_abbreviation("google.protobuf.Duration")
        .unwrap();

    let env = Env::with_standard_library()
        .with_proto_registry(Arc::new(registry))
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
    activation.insert(
        "event_time",
        Value::Timestamp(Timestamp::from_seconds(1704110400)),
    );
    activation.insert("timeout", Value::Duration(Duration::from_seconds(1800)));

    let result = program.eval(&activation);

    println!("Expression: event_time + timeout");
    println!("  event_time = 2024-01-01 12:00:00 UTC");
    println!("  timeout = 30 minutes");
    println!("Result: {}", result);
}
