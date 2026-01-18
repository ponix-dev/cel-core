//! Source information utilities for position tracking.

use google_cel_spec_community_neoeinstein_prost::cel::expr::SourceInfo;
use std::collections::HashMap;

/// Compute byte offsets of each line start.
///
/// Returns a vector where index i contains the byte offset where line i starts.
/// Line 0 always starts at offset 0.
pub fn compute_line_offsets(source: &str) -> Vec<i32> {
    let mut offsets = vec![0]; // First line starts at 0
    for (i, c) in source.char_indices() {
        if c == '\n' {
            offsets.push((i + 1) as i32);
        }
    }
    offsets
}

/// Build a SourceInfo from collected position data.
pub fn build_source_info(positions: HashMap<i64, i32>, line_offsets: Vec<i32>) -> SourceInfo {
    SourceInfo {
        syntax_version: String::new(),
        location: String::new(),
        line_offsets,
        positions,
        macro_calls: HashMap::new(),
        extensions: vec![],
    }
}

/// Get the byte offset position for an expression ID from source info.
pub fn get_position(source_info: &SourceInfo, expr_id: i64) -> Option<usize> {
    source_info.positions.get(&expr_id).map(|&pos| pos as usize)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_line_offsets_empty() {
        let offsets = compute_line_offsets("");
        assert_eq!(offsets, vec![0]);
    }

    #[test]
    fn test_compute_line_offsets_single_line() {
        let offsets = compute_line_offsets("hello");
        assert_eq!(offsets, vec![0]);
    }

    #[test]
    fn test_compute_line_offsets_multiple_lines() {
        let offsets = compute_line_offsets("a\nb\nc");
        // Line 0 starts at 0, line 1 at 2 (after "a\n"), line 2 at 4 (after "b\n")
        assert_eq!(offsets, vec![0, 2, 4]);
    }

    #[test]
    fn test_compute_line_offsets_trailing_newline() {
        let offsets = compute_line_offsets("a\n");
        assert_eq!(offsets, vec![0, 2]);
    }

    #[test]
    fn test_get_position() {
        let mut positions = HashMap::new();
        positions.insert(1, 5);
        positions.insert(2, 10);
        let source_info = build_source_info(positions, vec![0]);

        assert_eq!(get_position(&source_info, 1), Some(5));
        assert_eq!(get_position(&source_info, 2), Some(10));
        assert_eq!(get_position(&source_info, 3), None);
    }
}
