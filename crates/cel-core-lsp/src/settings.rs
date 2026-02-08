//! Settings infrastructure for cel-core-lsp.
//!
//! This module provides support for loading and parsing settings.toml files
//! to configure CEL environments with custom variables, extensions, and proto support.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use cel_core::{ext, types::FunctionDecl, CelType, Env};
use cel_core_proto::ProstProtoRegistry;
use serde::Deserialize;

/// Root settings structure loaded from settings.toml.
#[derive(Debug, Default, Deserialize)]
pub struct Settings {
    /// Environment configuration.
    pub env: Option<EnvSettings>,
}

/// Environment settings for configuring the CEL Env.
#[derive(Debug, Default, Deserialize)]
pub struct EnvSettings {
    /// Container namespace for qualified name resolution.
    pub container: Option<String>,

    /// Extensions to enable: ["strings", "math", "encoders", "optionals", "all"]
    pub extensions: Option<Vec<String>>,

    /// Whether to use strong enum typing (default: true).
    pub strong_enums: Option<bool>,

    /// Variable declarations: name -> type string.
    /// Type strings are parsed using `parse_type_string`.
    pub variables: Option<HashMap<String, String>>,

    /// Abbreviations for qualified name shortcuts.
    pub abbreviations: Option<Vec<String>>,

    /// Proto configuration.
    pub proto: Option<ProtoSettings>,
}

/// Proto-specific settings.
#[derive(Debug, Default, Deserialize)]
pub struct ProtoSettings {
    /// Paths to file descriptor set files (.binpb).
    /// Paths are relative to the workspace root.
    pub descriptors: Vec<PathBuf>,
}

/// Parse a type string into a CelType.
///
/// Supports:
/// - Primitives: bool, int, uint, double, string, bytes
/// - Special: null, dyn, timestamp, duration
/// - Parameterized: list(T), map(K, V), optional(T)
/// - Message types: any.other.name
///
/// # Examples
///
/// ```ignore
/// use cel_core_lsp::settings::parse_type_string;
///
/// assert_eq!(parse_type_string("int").unwrap(), CelType::Int);
/// assert_eq!(parse_type_string("list(string)").unwrap(), CelType::list(CelType::String));
/// ```
pub fn parse_type_string(s: &str) -> Result<CelType, String> {
    let s = s.trim();

    // Handle parameterized types first
    if let Some(inner_start) = s.find('(') {
        if !s.ends_with(')') {
            return Err(format!("malformed type string: missing closing paren in '{}'", s));
        }

        let type_name = &s[..inner_start];
        let inner = &s[inner_start + 1..s.len() - 1];

        return match type_name {
            "list" => {
                let elem = parse_type_string(inner)?;
                Ok(CelType::list(elem))
            }
            "map" => {
                // Split on comma, respecting nested parens
                let (key_str, val_str) = split_map_types(inner)?;
                let key = parse_type_string(key_str)?;
                let val = parse_type_string(val_str)?;
                Ok(CelType::map(key, val))
            }
            "optional" => {
                let elem = parse_type_string(inner)?;
                Ok(CelType::optional(elem))
            }
            "type" => {
                let elem = parse_type_string(inner)?;
                Ok(CelType::type_of(elem))
            }
            "wrapper" => {
                let elem = parse_type_string(inner)?;
                Ok(CelType::wrapper(elem))
            }
            _ => Err(format!("unknown parameterized type: '{}'", type_name)),
        };
    }

    // Handle primitives and well-known types
    match s {
        "bool" => Ok(CelType::Bool),
        "int" => Ok(CelType::Int),
        "uint" => Ok(CelType::UInt),
        "double" => Ok(CelType::Double),
        "string" => Ok(CelType::String),
        "bytes" => Ok(CelType::Bytes),
        "null" => Ok(CelType::Null),
        "dyn" => Ok(CelType::Dyn),
        "timestamp" => Ok(CelType::Timestamp),
        "duration" => Ok(CelType::Duration),
        "error" => Ok(CelType::Error),
        // Empty string is an error
        "" => Err("empty type string".to_string()),
        // Anything else is a message type
        _ => Ok(CelType::message(s)),
    }
}

/// Split map type parameters respecting nested parentheses.
fn split_map_types(s: &str) -> Result<(&str, &str), String> {
    let mut depth = 0;
    let mut split_pos = None;

    for (i, c) in s.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                if split_pos.is_some() {
                    return Err(format!("map type has more than 2 parameters: '{}'", s));
                }
                split_pos = Some(i);
            }
            _ => {}
        }
    }

    match split_pos {
        Some(pos) => Ok((&s[..pos].trim(), &s[pos + 1..].trim())),
        None => Err(format!("map type must have 2 parameters: '{}'", s)),
    }
}

/// Load settings from a settings.toml file.
///
/// Returns default settings if the file doesn't exist or can't be parsed.
pub fn load_settings(path: &Path) -> Settings {
    match std::fs::read_to_string(path) {
        Ok(content) => match toml::from_str(&content) {
            Ok(settings) => settings,
            Err(e) => {
                eprintln!("Warning: failed to parse settings.toml: {}", e);
                Settings::default()
            }
        },
        Err(_) => Settings::default(),
    }
}

/// Load settings from a workspace root directory.
///
/// Looks for settings.toml in the given directory.
pub fn load_settings_from_workspace(workspace_root: &Path) -> Settings {
    load_settings(&workspace_root.join("settings.toml"))
}

/// Load proto registry from file descriptor set files specified in settings.
///
/// Returns None if no descriptors are configured or if loading fails.
pub fn load_proto_registry(
    settings: &Settings,
    workspace_root: &Path,
) -> Option<Arc<ProstProtoRegistry>> {
    let proto = settings.env.as_ref()?.proto.as_ref()?;
    if proto.descriptors.is_empty() {
        return None;
    }

    let mut registry = ProstProtoRegistry::new();
    for path in &proto.descriptors {
        let full_path = if path.is_absolute() {
            path.clone()
        } else {
            workspace_root.join(path)
        };

        match std::fs::read(&full_path) {
            Ok(bytes) => {
                if let Err(e) = registry.add_file_descriptor_set(&bytes) {
                    eprintln!(
                        "Warning: failed to load proto descriptor '{}': {}",
                        full_path.display(),
                        e
                    );
                }
            }
            Err(e) => {
                eprintln!(
                    "Warning: failed to read proto descriptor file '{}': {}",
                    full_path.display(),
                    e
                );
            }
        }
    }

    Some(Arc::new(registry))
}

/// Build a CEL Env from settings with proto support.
///
/// This creates an Env with the standard library, applies all settings,
/// and loads proto descriptors from the specified paths relative to workspace_root.
pub fn build_env_with_protos(settings: &Settings, workspace_root: &Path) -> Env {
    build_env_from_settings_impl(settings, Some(workspace_root))
}

/// Internal implementation for building Env from settings.
fn build_env_from_settings_impl(settings: &Settings, workspace_root: Option<&Path>) -> Env {
    let mut env = Env::with_standard_library();

    if let Some(ref env_settings) = settings.env {
        // Apply extensions
        if let Some(ref extensions) = env_settings.extensions {
            env = apply_extensions(env, extensions);
        }

        // Apply variables
        if let Some(ref variables) = env_settings.variables {
            for (name, type_str) in variables {
                match parse_type_string(type_str) {
                    Ok(cel_type) => {
                        env.add_variable(name, cel_type);
                    }
                    Err(e) => {
                        eprintln!("Warning: failed to parse type for variable '{}': {}", name, e);
                    }
                }
            }
        }

        // Apply container
        if let Some(ref container) = env_settings.container {
            env.set_container(container);
        }

        // Apply abbreviations
        if let Some(ref abbreviations) = env_settings.abbreviations {
            let mut abbrevs = cel_core::Abbreviations::new();
            for name in abbreviations {
                match abbrevs.clone().add(name) {
                    Ok(a) => abbrevs = a,
                    Err(e) => {
                        eprintln!("Warning: failed to add abbreviation '{}': {}", name, e);
                    }
                }
            }
            env = env.with_abbreviations(abbrevs);
        }

        // Apply strong_enums setting
        if let Some(false) = env_settings.strong_enums {
            env = env.with_legacy_enums();
        }
    }

    // Load proto registry if workspace_root is provided and descriptors are configured
    if let Some(root) = workspace_root {
        if let Some(registry) = load_proto_registry(settings, root) {
            env = env.with_proto_registry(registry);
        }
    }

    env
}

/// Apply extension libraries based on extension names.
fn apply_extensions(mut env: Env, extensions: &[String]) -> Env {
    for ext_name in extensions {
        match ext_name.as_str() {
            "all" => {
                env = env.with_all_extensions();
            }
            "strings" | "string" => {
                env = env.with_extension(ext::string_extension());
            }
            "math" => {
                env = env.with_extension(ext::math_extension());
            }
            "encoders" | "encoder" => {
                env = env.with_extension(ext::encoders_extension());
            }
            "optionals" | "optional" => {
                env = env.with_extension(ext::optionals_extension());
            }
            other => {
                eprintln!("Warning: unknown extension: '{}'", other);
            }
        }
    }
    env
}

/// Add protovalidate extension functions to an environment.
///
/// This adds the protovalidate-specific functions like isEmail, isUri, etc.
pub fn protovalidate_extension() -> Vec<FunctionDecl> {
    use cel_core::types::OverloadDecl;

    vec![
        // isEmail(string) -> bool
        FunctionDecl::new("isEmail").with_overload(OverloadDecl::method(
            "isEmail_string",
            vec![CelType::String], // receiver in params
            CelType::Bool,
        )),
        // isHostname(string) -> bool
        FunctionDecl::new("isHostname").with_overload(OverloadDecl::method(
            "isHostname_string",
            vec![CelType::String],
            CelType::Bool,
        )),
        // isIp(string) -> bool, isIp(string, int) -> bool
        FunctionDecl::new("isIp")
            .with_overload(OverloadDecl::method(
                "isIp_string",
                vec![CelType::String],
                CelType::Bool,
            ))
            .with_overload(OverloadDecl::method(
                "isIp_string_int",
                vec![CelType::String, CelType::Int],
                CelType::Bool,
            )),
        // isIpPrefix(string) -> bool, isIpPrefix(string, int) -> bool, isIpPrefix(string, int, bool) -> bool
        FunctionDecl::new("isIpPrefix")
            .with_overload(OverloadDecl::method(
                "isIpPrefix_string",
                vec![CelType::String],
                CelType::Bool,
            ))
            .with_overload(OverloadDecl::method(
                "isIpPrefix_string_int",
                vec![CelType::String, CelType::Int],
                CelType::Bool,
            ))
            .with_overload(OverloadDecl::method(
                "isIpPrefix_string_int_bool",
                vec![CelType::String, CelType::Int, CelType::Bool],
                CelType::Bool,
            )),
        // isUri(string) -> bool
        FunctionDecl::new("isUri").with_overload(OverloadDecl::method(
            "isUri_string",
            vec![CelType::String],
            CelType::Bool,
        )),
        // isUriRef(string) -> bool
        FunctionDecl::new("isUriRef").with_overload(OverloadDecl::method(
            "isUriRef_string",
            vec![CelType::String],
            CelType::Bool,
        )),
        // unique(list) -> bool
        FunctionDecl::new("unique").with_overload(OverloadDecl::method(
            "unique_list",
            vec![CelType::list(CelType::Dyn)],
            CelType::Bool,
        )),
        // isNan(double) -> bool
        FunctionDecl::new("isNan").with_overload(OverloadDecl::method(
            "isNan_double",
            vec![CelType::Double],
            CelType::Bool,
        )),
        // isInf(double) -> bool, isInf(double, int) -> bool
        FunctionDecl::new("isInf")
            .with_overload(OverloadDecl::method(
                "isInf_double",
                vec![CelType::Double],
                CelType::Bool,
            ))
            .with_overload(OverloadDecl::method(
                "isInf_double_int",
                vec![CelType::Double, CelType::Int],
                CelType::Bool,
            )),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_env_from_settings(settings: &Settings) -> Env {
        build_env_from_settings_impl(settings, None)
    }

    fn build_protovalidate_env() -> Env {
        Env::with_standard_library()
            .with_all_extensions()
            .with_extension(protovalidate_extension())
            .with_variable("this", CelType::Dyn)
            .with_variable("rules", CelType::Dyn)
            .with_variable("now", CelType::Timestamp)
    }

    #[test]
    fn parse_primitive_types() {
        assert_eq!(parse_type_string("bool").unwrap(), CelType::Bool);
        assert_eq!(parse_type_string("int").unwrap(), CelType::Int);
        assert_eq!(parse_type_string("uint").unwrap(), CelType::UInt);
        assert_eq!(parse_type_string("double").unwrap(), CelType::Double);
        assert_eq!(parse_type_string("string").unwrap(), CelType::String);
        assert_eq!(parse_type_string("bytes").unwrap(), CelType::Bytes);
    }

    #[test]
    fn parse_special_types() {
        assert_eq!(parse_type_string("null").unwrap(), CelType::Null);
        assert_eq!(parse_type_string("dyn").unwrap(), CelType::Dyn);
        assert_eq!(parse_type_string("timestamp").unwrap(), CelType::Timestamp);
        assert_eq!(parse_type_string("duration").unwrap(), CelType::Duration);
    }

    #[test]
    fn parse_list_type() {
        assert_eq!(
            parse_type_string("list(string)").unwrap(),
            CelType::list(CelType::String)
        );
        assert_eq!(
            parse_type_string("list(int)").unwrap(),
            CelType::list(CelType::Int)
        );
    }

    #[test]
    fn parse_nested_list() {
        assert_eq!(
            parse_type_string("list(list(int))").unwrap(),
            CelType::list(CelType::list(CelType::Int))
        );
    }

    #[test]
    fn parse_map_type() {
        assert_eq!(
            parse_type_string("map(string, int)").unwrap(),
            CelType::map(CelType::String, CelType::Int)
        );
    }

    #[test]
    fn parse_nested_map() {
        assert_eq!(
            parse_type_string("map(string, list(int))").unwrap(),
            CelType::map(CelType::String, CelType::list(CelType::Int))
        );
    }

    #[test]
    fn parse_optional_type() {
        assert_eq!(
            parse_type_string("optional(int)").unwrap(),
            CelType::optional(CelType::Int)
        );
    }

    #[test]
    fn parse_message_type() {
        assert_eq!(
            parse_type_string("google.protobuf.Timestamp").unwrap(),
            CelType::message("google.protobuf.Timestamp")
        );
        assert_eq!(
            parse_type_string("my.custom.Message").unwrap(),
            CelType::message("my.custom.Message")
        );
    }

    #[test]
    fn parse_with_whitespace() {
        assert_eq!(parse_type_string("  int  ").unwrap(), CelType::Int);
        assert_eq!(
            parse_type_string("map( string , int )").unwrap(),
            CelType::map(CelType::String, CelType::Int)
        );
    }

    #[test]
    fn parse_empty_string_fails() {
        assert!(parse_type_string("").is_err());
        assert!(parse_type_string("   ").is_err());
    }

    #[test]
    fn parse_malformed_fails() {
        assert!(parse_type_string("list(").is_err());
        assert!(parse_type_string("map(int)").is_err());
        assert!(parse_type_string("unknown_param(int)").is_err());
    }

    #[test]
    fn build_env_with_variables() {
        let settings = Settings {
            env: Some(EnvSettings {
                variables: Some(
                    [
                        ("x".to_string(), "int".to_string()),
                        ("name".to_string(), "string".to_string()),
                    ]
                    .into_iter()
                    .collect(),
                ),
                ..Default::default()
            }),
        };

        let env = build_env_from_settings(&settings);

        assert!(env.variables().contains_key("x"));
        assert_eq!(env.variables().get("x"), Some(&CelType::Int));
        assert!(env.variables().contains_key("name"));
        assert_eq!(env.variables().get("name"), Some(&CelType::String));
    }

    #[test]
    fn build_env_with_container() {
        let settings = Settings {
            env: Some(EnvSettings {
                container: Some("my.package".to_string()),
                ..Default::default()
            }),
        };

        let env = build_env_from_settings(&settings);
        assert_eq!(env.container(), "my.package");
    }

    #[test]
    fn build_env_with_extensions() {
        let settings = Settings {
            env: Some(EnvSettings {
                extensions: Some(vec!["strings".to_string(), "math".to_string()]),
                ..Default::default()
            }),
        };

        let env = build_env_from_settings(&settings);
        // String extension adds charAt
        assert!(env.functions().contains_key("charAt"));
        // Math extension adds math.greatest
        assert!(env.functions().contains_key("math.greatest"));
    }

    #[test]
    fn build_env_with_all_extensions() {
        let settings = Settings {
            env: Some(EnvSettings {
                extensions: Some(vec!["all".to_string()]),
                ..Default::default()
            }),
        };

        let env = build_env_from_settings(&settings);
        assert!(env.functions().contains_key("charAt"));
        assert!(env.functions().contains_key("math.greatest"));
        assert!(env.functions().contains_key("base64.encode"));
        assert!(env.functions().contains_key("optional.of"));
    }

    #[test]
    fn protovalidate_extension_functions() {
        let funcs = protovalidate_extension();
        let names: Vec<_> = funcs.iter().map(|f| f.name.as_str()).collect();

        assert!(names.contains(&"isEmail"));
        assert!(names.contains(&"isHostname"));
        assert!(names.contains(&"isIp"));
        assert!(names.contains(&"isIpPrefix"));
        assert!(names.contains(&"isUri"));
        assert!(names.contains(&"isUriRef"));
        assert!(names.contains(&"unique"));
        assert!(names.contains(&"isNan"));
        assert!(names.contains(&"isInf"));
    }

    #[test]
    fn protovalidate_env_has_variables() {
        let env = build_protovalidate_env();

        assert!(env.variables().contains_key("this"));
        assert!(env.variables().contains_key("rules"));
        assert!(env.variables().contains_key("now"));
        assert_eq!(env.variables().get("now"), Some(&CelType::Timestamp));
    }

    #[test]
    fn protovalidate_env_has_extension_functions() {
        let env = build_protovalidate_env();

        assert!(env.functions().contains_key("isEmail"));
        assert!(env.functions().contains_key("isUri"));
        assert!(env.functions().contains_key("unique"));
    }

    #[test]
    fn load_proto_registry_with_no_settings() {
        let settings = Settings::default();
        let result = load_proto_registry(&settings, std::path::Path::new("."));
        assert!(result.is_none());
    }

    #[test]
    fn load_proto_registry_with_empty_descriptors() {
        let settings = Settings {
            env: Some(EnvSettings {
                proto: Some(ProtoSettings {
                    descriptors: vec![],
                }),
                ..Default::default()
            }),
        };
        let result = load_proto_registry(&settings, std::path::Path::new("."));
        assert!(result.is_none());
    }

    #[test]
    fn build_env_with_protos_without_descriptors() {
        let settings = Settings::default();
        // Should work without error even with no descriptors
        let env = build_env_with_protos(&settings, std::path::Path::new("."));
        // Env should have standard library functions
        assert!(env.functions().contains_key("size"));
    }
}
