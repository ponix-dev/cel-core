# Roadmap Handoff

## Last Updated
2026-01-18

## Just Completed
- Initial project setup with parser, LSP, and conformance testing infrastructure
- Parser is complete with error recovery
- Basic LSP features working (diagnostics, hover, semantic tokens)
- Conformance test framework in place
- **Section 1.1: Node IDs in AST** - Node IDs are now tracked in the AST

## Next Up
- **Section 1.2: Macro Expansion**
- CEL macros (`all`, `exists`, `exists_one`, `map`, `filter`) need to expand to `Comprehension` nodes during parsing
- Should follow cel-go's approach (Option A from roadmap)
- This is required before evaluation can handle comprehensions

## Open Questions
- Whether to create separate `cel-types` crate or keep types in `cel-eval`
- How to handle proto message values (dependency on prost?)
