# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## v0.3.0 - 2026-02-08
#### Features
- add GitHub Actions CI/CD workflows with mise - (2fc0fc7) - Simon Rall
- fixes has inputs and field level completions - (f0549d8) - Simon Rall
- initial protovalidate this support - (e3d62b9) - Simon Rall
- initial type checker lsp support - (6b7688c) - Simon Rall
- add cel.block extension with slot-based variable binding - (18f75cc) - Simon Rall
- resolve qualified proto message types as type values in evaluator - (704955b) - Simon Rall
- add proto.hasExt and proto.getExt macro expansion - (ac0a46c) - Simon Rall
- add namespace and qualified identifier resolution - (0420d90) - Simon Rall
- add Any type semantic equality and map key numeric coercion - (01ce96b) - Simon Rall
- add enum-to-int cross-type equality and comparison - (785f46c) - Simon Rall
- fix conversion operator edge cases for CEL conformance - (5d107a0) - Simon Rall
- add base64.encode/decode runtime implementations - (9e32e37) - Simon Rall
- add WKT value coercion and fix proto converter struct IDs - (99ce143) - Simon Rall
- add legacy enum mode with strong_enums flag - (3d1dac6) - Simon Rall
- add strong enum typing with EnumValue representation and constructors - (f1e9731) - Simon Rall
- add two-variable macros, exhaustive comprehension eval, and map merge operator - (e6758d0) - Simon Rall
- improve type checker inference with scoped type params and null assignability - (e2b4502) - Simon Rall
- implement optional extension eval and optional chaining - (6441ec0) - Simon Rall
- implement math extension eval and cross-type numeric comparison - (32d2399) - Simon Rall
- implement string extension eval and namespaced function dispatch - (3abef01) - Simon Rall
- improve proto message type conformance and WKT handling - (cba209c) - Simon Rall
- complete error-as-value semantics for logical operators - (160b796) - Simon Rall
- add abbreviations support for namespace resolution - (338f77f) - Simon Rall
- add timestamp and duration evaluation support - (fc84cbe) - Simon Rall
- add CEL evaluation engine - (452a577) - Simon Rall
#### Bug Fixes
- ci - (be3d4b2) - Simon Rall
- cog version - (0701c68) - Simon Rall
- clippy issues - (5d2da3d) - Simon Rall
- use default rust profile to include rustfmt and clippy - (eed7f73) - Simon Rall
- replace deprecated macos-13 runner with macos-latest - (e8f7256) - Simon Rall
- type checking has statements - (2c9f9d5) - Simon Rall
- structural ObjectValue comparison for deterministic conformance tests - (2268da3) - Simon Rall
#### Documentation
- update handoff notes and docs for trait abstraction - (f54c91c) - Simon Rall
- various readme and claude updates - (d870f76) - Simon Rall
- update handoff notes and conformance baseline - (1574650) - Simon Rall
- update handoff notes and conformance baseline - (3d5651a) - Simon Rall
- update handoff notes and conformance baseline - (da18f71) - Simon Rall
- update README with ergonomic API examples - (8b9e56c) - Simon Rall
#### Tests
- adds lsp snapshot tests - (61353d1) - Simon Rall
#### Continuous Integration
- mise install - (d289c93) - Simon Rall
#### Refactoring
- move test code in to test modules - (14e66fa) - Simon Rall
- bundle imports and fix dead code warnings - (3742d25) - Simon Rall
- initial api cleanup - (38c7db1) - Simon Rall
- more cleanup - (1f9d4f8) - Simon Rall
- proto registry rename - (dbd31b1) - Simon Rall
- decouple prost/prost-reflect from cel-core via trait abstraction - (88ce2de) - Simon Rall
- break down evaluator.rs into focused submodules - (a74e043) - Simon Rall
- add From/TryFrom traits for Value type conversion - (4d058dd) - Simon Rall
#### Miscellaneous Chores
- (**version**) v0.3.0 - (2e736a7) - github-actions[bot]
- fmt - (d27e3af) - Simon Rall
- update handoff notes and conformance baseline - (c7921cd) - Simon Rall
- claude stuff - (1686bb4) - Simon Rall

- - -

## v0.3.0 - 2026-02-08
#### Features
- add GitHub Actions CI/CD workflows with mise - (2fc0fc7) - Simon Rall
- fixes has inputs and field level completions - (f0549d8) - Simon Rall
- initial protovalidate this support - (e3d62b9) - Simon Rall
- initial type checker lsp support - (6b7688c) - Simon Rall
- add cel.block extension with slot-based variable binding - (18f75cc) - Simon Rall
- resolve qualified proto message types as type values in evaluator - (704955b) - Simon Rall
- add proto.hasExt and proto.getExt macro expansion - (ac0a46c) - Simon Rall
- add namespace and qualified identifier resolution - (0420d90) - Simon Rall
- add Any type semantic equality and map key numeric coercion - (01ce96b) - Simon Rall
- add enum-to-int cross-type equality and comparison - (785f46c) - Simon Rall
- fix conversion operator edge cases for CEL conformance - (5d107a0) - Simon Rall
- add base64.encode/decode runtime implementations - (9e32e37) - Simon Rall
- add WKT value coercion and fix proto converter struct IDs - (99ce143) - Simon Rall
- add legacy enum mode with strong_enums flag - (3d1dac6) - Simon Rall
- add strong enum typing with EnumValue representation and constructors - (f1e9731) - Simon Rall
- add two-variable macros, exhaustive comprehension eval, and map merge operator - (e6758d0) - Simon Rall
- improve type checker inference with scoped type params and null assignability - (e2b4502) - Simon Rall
- implement optional extension eval and optional chaining - (6441ec0) - Simon Rall
- implement math extension eval and cross-type numeric comparison - (32d2399) - Simon Rall
- implement string extension eval and namespaced function dispatch - (3abef01) - Simon Rall
- improve proto message type conformance and WKT handling - (cba209c) - Simon Rall
- complete error-as-value semantics for logical operators - (160b796) - Simon Rall
- add abbreviations support for namespace resolution - (338f77f) - Simon Rall
- add timestamp and duration evaluation support - (fc84cbe) - Simon Rall
- add CEL evaluation engine - (452a577) - Simon Rall
#### Bug Fixes
- clippy issues - (5d2da3d) - Simon Rall
- use default rust profile to include rustfmt and clippy - (eed7f73) - Simon Rall
- replace deprecated macos-13 runner with macos-latest - (e8f7256) - Simon Rall
- type checking has statements - (2c9f9d5) - Simon Rall
- structural ObjectValue comparison for deterministic conformance tests - (2268da3) - Simon Rall
#### Documentation
- update handoff notes and docs for trait abstraction - (f54c91c) - Simon Rall
- various readme and claude updates - (d870f76) - Simon Rall
- update handoff notes and conformance baseline - (1574650) - Simon Rall
- update handoff notes and conformance baseline - (3d5651a) - Simon Rall
- update handoff notes and conformance baseline - (da18f71) - Simon Rall
- update README with ergonomic API examples - (8b9e56c) - Simon Rall
#### Tests
- adds lsp snapshot tests - (61353d1) - Simon Rall
#### Continuous Integration
- mise install - (d289c93) - Simon Rall
#### Refactoring
- move test code in to test modules - (14e66fa) - Simon Rall
- bundle imports and fix dead code warnings - (3742d25) - Simon Rall
- initial api cleanup - (38c7db1) - Simon Rall
- more cleanup - (1f9d4f8) - Simon Rall
- proto registry rename - (dbd31b1) - Simon Rall
- decouple prost/prost-reflect from cel-core via trait abstraction - (88ce2de) - Simon Rall
- break down evaluator.rs into focused submodules - (a74e043) - Simon Rall
- add From/TryFrom traits for Value type conversion - (4d058dd) - Simon Rall
#### Miscellaneous Chores
- fmt - (d27e3af) - Simon Rall
- update handoff notes and conformance baseline - (c7921cd) - Simon Rall
- claude stuff - (1686bb4) - Simon Rall

- - -

## v0.2.0 - 2026-01-26
#### Features
- add proto type support and unified Ast type - (e518b5b) - Simon Rall
- add extension library infrastructure - (fcf21d0) - Simon Rall
- initial checker impl - (8e073ad) - Simon Rall
- add optional chaining, backtick identifiers, raw triple strings, reserved word fields, and INT64_MIN support - (c4b034a) - Simon Rall
- add parameterized type system with cel-core-types crate - (96390a1) - Simon Rall
- add optional syntax support for lists, maps, and structs - (3d06035) - Simon Rall
- macro expansion - (60a11f5) - Simon Rall
#### Bug Fixes
- complete lexer support for CEL conformance tests - (1ba5e9c) - Simon Rall
- cleanup unused code - (cf3fc45) - Simon Rall
#### Documentation
- update README with new crate structure and usage examples - (824a6ca) - Simon Rall
- update roadmap for unified Env completion - (e3bdf1e) - Simon Rall
- readme - (e5082ef) - Simon Rall
#### Refactoring
- ast proto api - (c9fdef6) - Simon Rall
- <span style="background-color: #d73a49; color: white; padding: 2px 6px; border-radius: 3px; font-weight: bold; font-size: 0.85em;">BREAKING</span>consolidate internal crates into cel-core - (fb304d5) - Simon Rall
- dep graph - (5c1264b) - Simon Rall
- pr feedback - (87f6c97) - Simon Rall
- roadmap claude commands - (ac89b40) - Simon Rall
#### Miscellaneous Chores
- update gitignore - (fcad91d) - Simon Rall
- remove handoff.md - (1a03f3e) - Simon Rall

- - -

## v0.1.3 - 2026-01-19
#### Bug Fixes
- proto generate - (c069c1a) - Simon Rall

- - -

## v0.1.2 - 2026-01-19
#### Refactoring
- proto generation - (7b19b8c) - Simon Rall

- - -

## v0.1.1 - 2026-01-19
#### Bug Fixes
- cog - (6a62a5e) - Simon Rall
- semantic versioning - (9d49ba2) - Simon Rall

- - -

## v0.1.1 - 2026-01-19
#### Bug Fixes
- cog - (6a62a5e) - Simon Rall
- semantic versioning - (9d49ba2) - Simon Rall

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).