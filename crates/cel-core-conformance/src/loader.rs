//! Load .textproto test files using prost-reflect.
//!
//! This module handles parsing the CEL conformance test data files
//! from the cel-spec repository.

use prost::Message;
use prost_reflect::{DescriptorPool, DynamicMessage};
use std::path::Path;

use google_cel_spec_community_neoeinstein_prost::cel::expr::{
    conformance,
    conformance::test::SimpleTestFile,
    FILE_DESCRIPTOR_SET as EXPR_FILE_DESCRIPTOR_SET,
};

/// Load a SimpleTestFile from a .textproto file.
pub fn load_test_file(path: &Path) -> Result<SimpleTestFile, LoadError> {
    // Start with the global pool which contains Google well-known types
    let mut pool = DescriptorPool::global();

    // Add the cel-spec descriptors in dependency order:
    // 1. cel/expr/*.proto (syntax.proto, checked.proto, etc.)
    pool.decode_file_descriptor_set(EXPR_FILE_DESCRIPTOR_SET)
        .map_err(|e| LoadError::DescriptorPool(e.to_string()))?;

    // 2. cel/expr/conformance/*.proto (conformance_service.proto)
    pool.decode_file_descriptor_set(conformance::FILE_DESCRIPTOR_SET)
        .map_err(|e| LoadError::DescriptorPool(e.to_string()))?;

    // 3. cel/expr/conformance/proto2/*.proto (test types)
    pool.decode_file_descriptor_set(conformance::proto2::FILE_DESCRIPTOR_SET)
        .map_err(|e| LoadError::DescriptorPool(e.to_string()))?;

    // 4. cel/expr/conformance/proto3/*.proto (test types)
    pool.decode_file_descriptor_set(conformance::proto3::FILE_DESCRIPTOR_SET)
        .map_err(|e| LoadError::DescriptorPool(e.to_string()))?;

    // 5. cel/expr/conformance/test/*.proto (simple.proto)
    pool.decode_file_descriptor_set(conformance::test::FILE_DESCRIPTOR_SET)
        .map_err(|e| LoadError::DescriptorPool(e.to_string()))?;

    let message_desc = pool
        .get_message_by_name("cel.expr.conformance.test.SimpleTestFile")
        .ok_or_else(|| LoadError::MessageNotFound("SimpleTestFile".to_string()))?;

    let content =
        std::fs::read_to_string(path).map_err(|e| LoadError::FileRead(path.to_path_buf(), e))?;

    let dynamic = DynamicMessage::parse_text_format(message_desc, &content)
        .map_err(|e| LoadError::TextFormat(e.to_string()))?;

    let bytes = dynamic.encode_to_vec();
    SimpleTestFile::decode(bytes.as_slice()).map_err(|e| LoadError::Decode(e.to_string()))
}

/// Errors that can occur when loading test files.
#[derive(Debug)]
pub enum LoadError {
    /// Failed to decode the file descriptor set.
    DescriptorPool(String),
    /// Message type not found in descriptor pool.
    MessageNotFound(String),
    /// Failed to read the file.
    FileRead(std::path::PathBuf, std::io::Error),
    /// Failed to parse text format.
    TextFormat(String),
    /// Failed to decode the protobuf message.
    Decode(String),
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::DescriptorPool(e) => write!(f, "failed to decode descriptor pool: {}", e),
            LoadError::MessageNotFound(name) => write!(f, "message type not found: {}", name),
            LoadError::FileRead(path, e) => write!(f, "failed to read {}: {}", path.display(), e),
            LoadError::TextFormat(e) => write!(f, "failed to parse text format: {}", e),
            LoadError::Decode(e) => write!(f, "failed to decode protobuf: {}", e),
        }
    }
}

impl std::error::Error for LoadError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list_messages() {
        let mut pool = DescriptorPool::global();
        println!("Global pool messages before: {}", pool.all_messages().count());

        // Add descriptors in dependency order
        pool.decode_file_descriptor_set(EXPR_FILE_DESCRIPTOR_SET)
            .expect("Failed to decode expr descriptors");
        pool.decode_file_descriptor_set(conformance::FILE_DESCRIPTOR_SET)
            .expect("Failed to decode conformance descriptors");
        pool.decode_file_descriptor_set(conformance::proto2::FILE_DESCRIPTOR_SET)
            .expect("Failed to decode proto2 test descriptors");
        pool.decode_file_descriptor_set(conformance::proto3::FILE_DESCRIPTOR_SET)
            .expect("Failed to decode proto3 test descriptors");
        pool.decode_file_descriptor_set(conformance::test::FILE_DESCRIPTOR_SET)
            .expect("Failed to decode test descriptors");

        println!("Global pool messages after: {}", pool.all_messages().count());

        let messages: Vec<_> = pool.all_messages().map(|m| m.full_name().to_string()).collect();
        println!("Available messages ({}):", messages.len());
        for msg in &messages {
            if msg.contains("Simple") || msg.contains("Test") || msg.contains("cel") {
                println!("  - {}", msg);
            }
        }
    }

    #[test]
    fn test_load_basic_textproto() {
        let path = Path::new("cel-spec/tests/simple/testdata/basic.textproto");
        let result = load_test_file(path);
        assert!(result.is_ok(), "Failed to load basic.textproto: {:?}", result.err());

        let test_file = result.unwrap();
        assert_eq!(test_file.name, "basic");
        assert!(!test_file.section.is_empty(), "Expected at least one section");
    }

    #[test]
    fn test_load_parse_textproto() {
        let path = Path::new("cel-spec/tests/simple/testdata/parse.textproto");
        let result = load_test_file(path);
        assert!(result.is_ok(), "Failed to load parse.textproto: {:?}", result.err());

        let test_file = result.unwrap();
        assert_eq!(test_file.name, "parse");
    }
}
