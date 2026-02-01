# Performance Benchmarking Plan

## Goal

Add criterion-based performance benchmarks and dhat-based allocation profiling, mirroring cel-go's `test/bench/` reference cases. Rename `cel-core-conformance` to `cel-core-testing` to house both conformance tests and benchmarks.

## Plan File

The full implementation plan is at: `.claude/plans/sprightly-wandering-manatee.md`

## How to Use

Tell Claude Code:

> Implement the performance benchmarking plan in `.claude/plans/sprightly-wandering-manatee.md`

The plan covers:

1. **Rename crate** — `cel-core-conformance` → `cel-core-testing` (directory, Cargo.toml, .gitmodules, mise.toml, imports)
2. **Add criterion benchmarks** — `benches/cel_benchmarks.rs` with all 13 cel-go reference cases across parse/compile/eval phases
3. **Add dhat allocation profiling** — `benches/cel_allocations.rs` behind a `dhat-heap` feature flag (separate from criterion to avoid timing interference)
4. **Add mise tasks** — `mise run bench` and `mise run bench:alloc`
5. **Update documentation** — README.md, CLAUDE.md, and ROADMAP.md with new crate name and benchmark commands

## Key Design Decisions Already Made

- **dhat is incompatible with criterion** — dhat replaces the global allocator with ~2-10x overhead, so it gets a separate bench target gated behind `required-features = ["dhat-heap"]`
- **All 13 cel-go reference cases can be replicated** — string ops, list membership, exists/filter with regex, string formatting
- **cel-go's optimized/unoptimized/trace modes don't map to cel-core** — instead we benchmark parse, compile, and eval phases separately
- **Criterion for speed, dhat for allocations** — `cargo bench` runs only criterion; dhat requires `--features dhat-heap --bench cel_allocations`
