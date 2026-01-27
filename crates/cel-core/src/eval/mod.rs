//! CEL Evaluation Engine.
//!
//! This module provides the runtime evaluation infrastructure for CEL expressions.
//! It follows the cel-go architecture pattern where:
//!
//! - `Value` represents runtime values
//! - `Activation` provides variable bindings
//! - `Program` wraps a compiled expression with its function registry
//! - `Evaluator` performs tree-walking evaluation
//!
//! # Example
//!
//! ```
//! use cel_core::{Env, CelType};
//! use cel_core::eval::{Value, MapActivation, Activation};
//!
//! let env = Env::with_standard_library()
//!     .with_variable("x", CelType::Int);
//!
//! let ast = env.compile("x + 1").unwrap();
//! let program = env.program(&ast).unwrap();
//!
//! let mut activation = MapActivation::new();
//! activation.insert("x", Value::Int(41));
//!
//! let result = program.eval(&activation);
//! assert_eq!(result, Value::Int(42));
//! ```

mod activation;
mod error;
mod evaluator;
mod functions;
mod program;
mod value;

pub use activation::{
    Activation, EmptyActivation, HierarchicalActivation, MapActivation, SharedActivation,
};
pub use error::{EvalError, EvalErrorKind};
pub use evaluator::Evaluator;
pub use functions::{Function, FunctionImpl, FunctionRegistry, Overload};
pub use program::Program;
pub use value::{Duration, MapKey, OptionalValue, Timestamp, TypeValue, Value, ValueMap};
