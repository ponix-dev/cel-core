# Roadmap Handoff

## Last Updated
2026-02-08

## Just Completed
- GitHub Issue: #60 — Automate releases with GitHub Actions
- [x] Created reusable composite action `.github/actions/setup/action.yml` using `jdx/mise-action@v3` + `Swatinem/rust-cache@v2`
- [x] Created `.github/workflows/pr.yml` — runs fmt, clippy, test on pull requests to main
- [x] Created `.github/workflows/main.yml` — runs CI checks then gates release pipeline (check-bump → bump → publish + build-binaries → release)
- [x] All tool installation (rust, cocogitto, cargo-edit) handled via mise with caching
- [x] Removed standalone `cocogitto/cocogitto-action@v4` and `cargo install cargo-edit` in favor of mise
- [x] Release pipeline gated on CI passing (check-bump depends on fmt/clippy/test)

### Summary
Restructured GitHub Actions from `ci.yml` + `release.yml` into `pr.yml` (PR trigger) and `main.yml` (main push trigger). Extracted shared Rust toolchain setup into a reusable composite action that uses mise for dependency management with caching. The release pipeline on main is now gated behind CI checks passing.

### Key files added
- `.github/actions/setup/action.yml` — composite action: mise + cargo cache
- `.github/workflows/pr.yml` — PR CI: fmt, clippy, test (3 OS matrix)
- `.github/workflows/main.yml` — main CI + release: fmt, clippy, test → check-bump → bump → publish + build-binaries → release

### Notable decisions
- Two-layer caching: `jdx/mise-action@v3` caches tool installations, `Swatinem/rust-cache@v2` caches cargo build artifacts
- Kept build matrix (ubuntu, macos-13, macos-14) for both test and binary builds
- Release jobs only run when a version bump is detected by cocogitto

## Previous Work
- GitHub Issue: #59 — Clean up public API surface and export patterns
- GitHub Issue: #35 — Replace custom LSP validation with cel-core's checker
- GitHub Issue: #50 — Decoupled prost/prost-reflect from cel-core via trait abstraction

## Next Up
- GitHub Issue: #58 — Macro calls with wrong argument count produce misleading 'undeclared reference' error
  - Parser/checker bug where bad macro arity gives confusing error messages
  - Should be a focused fix in the macro expansion or checker error reporting
- GitHub Issue: #38 — Richer hover information using CheckResult.type_map
  - Now that the LSP uses the checker, hover can show precise types from the type map
  - Natural follow-up since the infrastructure is already in place
- GitHub Issue: #48 — Move proto value conversion logic from conformance layer to cel-core-proto
  - Clean separation of concerns, consolidate proto conversion in one place

## Open Questions
- The overload resolution sometimes selects the wrong overload for `(UInt, Int)` args in bit shift functions — it picks `int_int` instead of `uint_int`. Worked around by handling both type combos in the first overload, but root cause may need investigation.
- Completion for map types could be improved — currently only shows methods, not key access patterns
- Settings discovery could be enhanced to walk up directory tree (currently only checks workspace root)
