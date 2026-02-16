# Roadmap Handoff

## Last Updated
2026-02-15

## Just Completed
- GitHub Issue: #65 — Walk file tree to discover settings.toml
- [x] Added `discover_settings()` that walks up the directory tree, then checks child directories
- [x] Updated `DocumentStore::open()` to accept an optional `Env` for `.cel` files
- [x] Added `env: OnceLock<Arc<Env>>` to `Backend` so settings-configured env is used for all documents
- [x] Updated `initialize()` to use `discover_settings` instead of `load_settings_from_workspace`
- [x] Added 5 unit tests for discovery and 1 integration test for end-to-end `.cel` file settings

### Summary
Previously, settings.toml was only looked for in the workspace root, and even when found, its configuration was only applied to `.proto` files — `.cel` files always got a hardcoded default env. Now the LSP walks up the directory tree from the workspace root to find settings.toml (with a fallback to check immediate child directories), and the discovered settings are applied to both `.cel` and `.proto` files.

### Key files
- **Modified:** `crates/cel-core-lsp/src/settings.rs` — added `discover_settings()`, removed unused `load_settings_from_workspace()`, added 5 unit tests
- **Modified:** `crates/cel-core-lsp/src/document/state.rs` — `DocumentStore::open()` now accepts `env: Option<&Arc<Env>>`
- **Modified:** `crates/cel-core-lsp/src/lib.rs` — added `env` field to `Backend`, updated `initialize()` and `on_document_change()`
- **Modified:** `crates/cel-core-lsp/tests/integration.rs` — added `discover_settings_applies_to_cel_files` test

### Notable decisions
- Two-phase search: walk up first (prioritizes parent directories), then check immediate children as fallback
- Returns `(Settings, PathBuf)` where the PathBuf is the settings directory, used for resolving relative descriptor paths
- Removed `load_settings_from_workspace` as dead code — fully replaced by `discover_settings`

## Previous Work
- GitHub Issue: #66 — Show variable types on hover
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
- GitHub Issue: #58 — Macro calls with wrong argument count produce misleading 'undeclared reference' error
- GitHub Issue: #60 — Automate releases with GitHub Actions
- GitHub Issue: #59 — Clean up public API surface and export patterns
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #56 — Auto-discover buf dependencies for LSP proto registry
  - Natural follow-up: now that settings discovery walks the tree, auto-discovering buf.yaml dependencies would reduce manual descriptor configuration
- GitHub Issue: #44 — LSP workspace/configuration support
  - Could build on discover_settings to support dynamic workspace configuration changes

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but root cause may need investigation.
- Completion for map types could be improved — currently only shows methods, not key access patterns
