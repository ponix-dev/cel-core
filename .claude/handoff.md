# Roadmap Handoff

## Last Updated
2026-02-15

## Just Completed
- GitHub Issue: #71 — Split cel-core-lsp into separate celsp repository
- [x] Created `ponix-dev/celsp` GitHub repository
- [x] Copied all LSP source code and tests to new repo as standalone crate
- [x] Renamed package from `cel-core-lsp` to `celsp` with crates.io dependencies on cel-core 0.4.x
- [x] Set up CI (pr.yml, main.yml with binary builds + releases for 3 targets)
- [x] Added mise.toml, cog.toml, README.md, licenses, .gitignore
- [x] Removed `crates/cel-core-lsp/` from cel-core workspace
- [x] Updated cel-core CI to remove build-binaries and release jobs
- [x] Updated mise.toml, README.md, and CLAUDE.md to remove LSP references

### Summary
The LSP (`cel-core-lsp`) was extracted into its own repository at `ponix-dev/celsp` so it can be versioned and released independently from the core library. The celsp repo uses crates.io dependencies on `cel-core 0.4` and `cel-core-proto 0.4`. All 124 LSP tests pass in the new repo. The cel-core repo's CI was simplified to only handle library publishing (no more binary builds or GitHub releases with binary artifacts).

### Key changes in cel-core
- **Deleted:** `crates/cel-core-lsp/` — entire directory
- **Modified:** `.github/workflows/main.yml` — removed `build-binaries` and `release` jobs
- **Modified:** `mise.toml` — removed `install-lsp` task
- **Modified:** `README.md` — replaced LSP section with link to celsp, updated crate table
- **Modified:** `CLAUDE.md` — updated project description, removed LSP architecture section

### Key files in celsp repo
- `Cargo.toml` — standalone package with crates.io deps
- `src/` — all source from cel-core-lsp (references updated to `celsp`)
- `tests/` — all tests and fixtures
- `.github/workflows/main.yml` — CI with binary builds + release
- `.github/workflows/pr.yml` — PR checks
- `mise.toml`, `cog.toml`, `README.md`, licenses

## Previous Work
- GitHub Issue: #65 — Walk file tree to discover settings.toml
- GitHub Issue: #66 — Show variable types on hover
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
- GitHub Issue: #58 — Macro calls with wrong argument count produce misleading 'undeclared reference' error
- GitHub Issue: #60 — Automate releases with GitHub Actions
- GitHub Issue: #59 — Clean up public API surface and export patterns
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #56 — Auto-discover buf dependencies for LSP proto registry
  - Now lives in celsp repo — natural follow-up to settings discovery
- GitHub Issue: #44 — LSP workspace/configuration support
  - Also lives in celsp repo now

## Open Questions
- celsp repo needs its initial push to GitHub (commits are ready locally at `/Users/srall/development/celsp`)
- celsp will need secrets configured for any future crates.io publishing
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — tracked separately
