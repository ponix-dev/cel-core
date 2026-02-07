//! Document state management and text utilities.
//!
//! This module provides:
//! - `LineIndex` for efficient byte offset <-> LSP position conversion
//! - `CelRegion` and `OffsetMapper` for embedded CEL in host documents
//! - `DocumentState` and `DocumentStore` for document lifecycle management

mod region;
mod state;
mod text;

pub use region::{CelRegion, OffsetMapper};
pub use state::{DocumentKind, DocumentState, DocumentStore, ProtoDocumentState};
pub use text::LineIndex;
