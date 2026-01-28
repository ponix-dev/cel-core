//! Using containers and abbreviations together.
//!
//! Run with: cargo run -p cel-core --example combined

use cel_core::eval::{Duration, MapActivation, Timestamp, Value};
use cel_core::types::ProtoTypeRegistry;
use cel_core::{Abbreviations, CelType, Env};

fn main() {
    let registry = ProtoTypeRegistry::new();

    // Container for our domain, abbreviations for well-known types
    let abbrevs = Abbreviations::from_qualified_names(&[
        "google.protobuf.Timestamp",
        "google.protobuf.Duration",
    ])
    .unwrap();

    let env = Env::with_standard_library()
        .with_proto_types(registry)
        .with_container("myapp.models")
        .with_abbreviations(abbrevs)
        .with_variable("request_time", CelType::Timestamp)
        .with_variable("max_age", CelType::Duration)
        .with_variable("now", CelType::Timestamp);

    println!("Container: myapp.models");
    println!("Abbreviations: Timestamp, Duration\n");

    // Construct proto message with abbreviated type name
    let ast = env.compile("Timestamp{seconds: 1704067200, nanos: 0}").unwrap();
    let program = env.program(&ast).unwrap();
    let result = program.eval(&MapActivation::new());

    println!("Proto construction with abbreviation:");
    println!("  Timestamp{{seconds: 1704067200, nanos: 0}}");
    println!("  Result: {}\n", result);

    // Check if cache is still valid using CEL timestamp arithmetic
    let ast = env.compile("now < request_time + max_age").unwrap();
    let program = env.program(&ast).unwrap();

    let mut activation = MapActivation::new();
    activation.insert("request_time", Value::Timestamp(Timestamp::from_seconds(1704110400)));
    activation.insert("max_age", Value::Duration(Duration::from_seconds(3600)));
    activation.insert("now", Value::Timestamp(Timestamp::from_seconds(1704112200)));

    let result = program.eval(&activation);

    println!("Cache validity check:");
    println!("  Expression: now < request_time + max_age");
    println!("  request_time = 2024-01-01 12:00:00 UTC");
    println!("  max_age = 1 hour");
    println!("  now = 2024-01-01 12:30:00 UTC");
    println!("  Result: {} (cache still valid)\n", result);

    // Calculate remaining time
    let ast = env.compile("request_time + max_age - now").unwrap();
    let program = env.program(&ast).unwrap();
    let result = program.eval(&activation);

    println!("Time remaining:");
    println!("  Expression: request_time + max_age - now");
    println!("  Result: {}", result);
}
