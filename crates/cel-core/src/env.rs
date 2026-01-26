//! Unified environment for CEL expression processing.
//!
//! The `Env` struct coordinates parse, check, and (future) eval operations,
//! following the cel-go architecture pattern.

use std::collections::HashMap;

use cel_core_checker::{check, CheckResult, FunctionDecl, STANDARD_LIBRARY};
use cel_core_common::{extensions, CelType, SpannedExpr};
use cel_core_parser::ParseResult;

/// Unified environment for CEL expression processing.
///
/// The `Env` wraps the parser and checker, providing a high-level API
/// for working with CEL expressions. It manages:
/// - Variable declarations with their types
/// - Function declarations (standard library + extensions)
/// - Container namespace for qualified name resolution
///
/// # Example
///
/// ```
/// use cel_core::Env;
/// use cel_core_common::CelType;
///
/// let env = Env::with_standard_library()
///     .with_variable("x", CelType::Int);
///
/// let result = env.compile("x + 1");
/// assert!(result.is_ok());
/// ```
#[derive(Debug, Clone)]
pub struct Env {
    /// Variable declarations (name -> type).
    variables: HashMap<String, CelType>,
    /// Function declarations indexed by name.
    functions: HashMap<String, FunctionDecl>,
    /// Container namespace for qualified name resolution.
    container: String,
}

impl Env {
    /// Create a new empty environment.
    ///
    /// This environment has no functions or variables defined.
    /// Use `with_standard_library()` for a fully-featured environment.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            container: String::new(),
        }
    }

    /// Create a new environment with the CEL standard library.
    ///
    /// This includes all standard operators, functions, and type constants.
    pub fn with_standard_library() -> Self {
        let mut env = Self::new();

        // Add standard library functions
        for func in STANDARD_LIBRARY.iter() {
            env.functions.insert(func.name.clone(), func.clone());
        }

        // Add type constants (type values for type checking)
        env.add_type_constant("bool", CelType::Bool);
        env.add_type_constant("int", CelType::Int);
        env.add_type_constant("uint", CelType::UInt);
        env.add_type_constant("double", CelType::Double);
        env.add_type_constant("string", CelType::String);
        env.add_type_constant("bytes", CelType::Bytes);
        env.add_type_constant("list", CelType::list(CelType::Dyn));
        env.add_type_constant("map", CelType::map(CelType::Dyn, CelType::Dyn));
        env.add_type_constant("null_type", CelType::Null);
        env.add_type_constant("type", CelType::type_of(CelType::Dyn));
        env.add_type_constant("dyn", CelType::Dyn);

        env
    }

    /// Add a type constant to the environment.
    fn add_type_constant(&mut self, name: &str, cel_type: CelType) {
        self.variables
            .insert(name.to_string(), CelType::type_of(cel_type));
    }

    /// Add a variable to the environment (builder pattern).
    ///
    /// # Example
    ///
    /// ```
    /// use cel_core::Env;
    /// use cel_core_common::CelType;
    ///
    /// let env = Env::with_standard_library()
    ///     .with_variable("x", CelType::Int)
    ///     .with_variable("y", CelType::String);
    /// ```
    pub fn with_variable(mut self, name: impl Into<String>, cel_type: CelType) -> Self {
        self.variables.insert(name.into(), cel_type);
        self
    }

    /// Add a variable to the environment (mutable).
    pub fn add_variable(&mut self, name: impl Into<String>, cel_type: CelType) {
        self.variables.insert(name.into(), cel_type);
    }

    /// Add a function declaration to the environment (builder pattern).
    pub fn with_function(mut self, decl: FunctionDecl) -> Self {
        self.add_function(decl);
        self
    }

    /// Add a function declaration to the environment (mutable).
    ///
    /// If a function with the same name already exists, overloads are merged.
    pub fn add_function(&mut self, decl: FunctionDecl) {
        if let Some(existing) = self.functions.get_mut(&decl.name) {
            // Merge overloads
            existing.overloads.extend(decl.overloads);
        } else {
            self.functions.insert(decl.name.clone(), decl);
        }
    }

    /// Set the container namespace (builder pattern).
    ///
    /// The container is used for qualified name resolution.
    pub fn with_container(mut self, container: impl Into<String>) -> Self {
        self.container = container.into();
        self
    }

    /// Set the container namespace (mutable).
    pub fn set_container(&mut self, container: impl Into<String>) {
        self.container = container.into();
    }

    /// Get the container namespace.
    pub fn container(&self) -> &str {
        &self.container
    }

    /// Add an extension library to the environment (builder pattern).
    ///
    /// Extensions provide additional functions beyond the standard library.
    /// Each extension is a collection of `FunctionDecl` values.
    ///
    /// # Example
    ///
    /// ```
    /// use cel_core::Env;
    /// use cel_core_common::extensions::string_extension;
    ///
    /// let env = Env::with_standard_library()
    ///     .with_extension(string_extension());
    /// ```
    pub fn with_extension(mut self, extension: impl IntoIterator<Item = FunctionDecl>) -> Self {
        for decl in extension {
            self.add_function(decl);
        }
        self
    }

    /// Add all available extension libraries to the environment (builder pattern).
    ///
    /// This is a convenience method that adds all standard extensions:
    /// - String extension (`charAt`, `indexOf`, `substring`, etc.)
    /// - Math extension (`math.greatest`, `math.least`, `math.abs`, etc.)
    /// - Encoders extension (`base64.encode`, `base64.decode`)
    /// - Optionals extension (`optional.of`, `optional.none`, `hasValue`, etc.)
    ///
    /// # Example
    ///
    /// ```
    /// use cel_core::Env;
    ///
    /// let env = Env::with_standard_library()
    ///     .with_all_extensions();
    /// ```
    pub fn with_all_extensions(mut self) -> Self {
        self = self
            .with_extension(extensions::string_extension())
            .with_extension(extensions::math_extension())
            .with_extension(extensions::encoders_extension())
            .with_extension(extensions::optionals_extension());

        // Add optional_type type constant for the optionals extension
        self.add_type_constant("optional_type", CelType::optional(CelType::Dyn));

        self
    }

    /// Get the variables map.
    pub fn variables(&self) -> &HashMap<String, CelType> {
        &self.variables
    }

    /// Get the functions map.
    pub fn functions(&self) -> &HashMap<String, FunctionDecl> {
        &self.functions
    }

    /// Parse a CEL expression.
    ///
    /// This delegates to the parser. The returned `ParseResult` may contain
    /// both a partial AST and errors if parsing partially succeeded.
    pub fn parse(&self, source: &str) -> ParseResult {
        cel_core_parser::parse(source)
    }

    /// Type-check a parsed expression.
    ///
    /// This delegates to the checker with the environment's variables,
    /// functions, and container.
    pub fn check(&self, expr: &SpannedExpr) -> CheckResult {
        check(expr, &self.variables, &self.functions, &self.container)
    }

    /// Parse and type-check a CEL expression.
    ///
    /// This is a convenience method that combines `parse()` and `check()`.
    /// Returns the check result if parsing succeeded, or an error if parsing failed.
    pub fn compile(&self, source: &str) -> Result<CheckResult, ParseResult> {
        let parse_result = self.parse(source);

        if let Some(ref ast) = parse_result.ast {
            if parse_result.errors.is_empty() {
                return Ok(self.check(ast));
            }
        }

        Err(parse_result)
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cel_core_checker::{CheckErrorKind, OverloadDecl};

    #[test]
    fn test_new_env() {
        let env = Env::new();
        assert!(env.variables().is_empty());
        assert!(env.functions().is_empty());
    }

    #[test]
    fn test_with_standard_library() {
        let env = Env::with_standard_library();

        // Should have standard functions
        assert!(env.functions().contains_key("_+_"));
        assert!(env.functions().contains_key("size"));
        assert!(env.functions().contains_key("contains"));

        // Should have type constants
        assert!(env.variables().contains_key("bool"));
        assert!(env.variables().contains_key("int"));
    }

    #[test]
    fn test_with_variable() {
        let env = Env::with_standard_library().with_variable("x", CelType::Int);

        assert!(env.variables().contains_key("x"));
        assert_eq!(env.variables().get("x"), Some(&CelType::Int));
    }

    #[test]
    fn test_parse() {
        let env = Env::new();
        let result = env.parse("1 + 2");

        assert!(result.ast.is_some());
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_check() {
        let env = Env::with_standard_library().with_variable("x", CelType::Int);

        let parse_result = env.parse("x + 1");
        let ast = parse_result.ast.unwrap();

        let check_result = env.check(&ast);
        assert!(check_result.is_ok());
    }

    #[test]
    fn test_check_undefined_variable() {
        let env = Env::with_standard_library();

        let parse_result = env.parse("x + 1");
        let ast = parse_result.ast.unwrap();

        let check_result = env.check(&ast);
        assert!(!check_result.is_ok());
        assert!(check_result.errors.iter().any(|e| matches!(
            &e.kind,
            CheckErrorKind::UndeclaredReference { name, .. } if name == "x"
        )));
    }

    #[test]
    fn test_compile_success() {
        let env = Env::with_standard_library().with_variable("x", CelType::Int);

        let result = env.compile("x + 1");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_compile_parse_error() {
        let env = Env::with_standard_library();

        let result = env.compile("1 +");
        assert!(result.is_err());
    }

    #[test]
    fn test_container() {
        let env = Env::with_standard_library().with_container("google.protobuf");

        assert_eq!(env.container(), "google.protobuf");
    }

    #[test]
    fn test_add_function() {
        let mut env = Env::new();

        let func = FunctionDecl::new("custom").with_overload(OverloadDecl::function(
            "custom_int",
            vec![CelType::Int],
            CelType::Bool,
        ));

        env.add_function(func);

        assert!(env.functions().contains_key("custom"));
    }

    #[test]
    fn test_with_extension() {
        use cel_core_common::extensions::string_extension;

        let env = Env::with_standard_library().with_extension(string_extension());

        // String extension should add charAt function
        assert!(env.functions().contains_key("charAt"));
        assert!(env.functions().contains_key("indexOf"));
        assert!(env.functions().contains_key("substring"));
    }

    #[test]
    fn test_with_all_extensions() {
        let env = Env::with_standard_library().with_all_extensions();

        // String extension functions
        assert!(env.functions().contains_key("charAt"));
        assert!(env.functions().contains_key("indexOf"));
        assert!(env.functions().contains_key("join"));
        assert!(env.functions().contains_key("strings.quote"));

        // Math extension functions
        assert!(env.functions().contains_key("math.greatest"));
        assert!(env.functions().contains_key("math.least"));
        assert!(env.functions().contains_key("math.abs"));
        assert!(env.functions().contains_key("math.bitAnd"));

        // Encoders extension functions
        assert!(env.functions().contains_key("base64.encode"));
        assert!(env.functions().contains_key("base64.decode"));

        // Optionals extension functions
        assert!(env.functions().contains_key("optional.of"));
        assert!(env.functions().contains_key("optional.none"));
        assert!(env.functions().contains_key("optional.ofNonZeroValue"));
        assert!(env.functions().contains_key("hasValue"));
        assert!(env.functions().contains_key("value"));
        assert!(env.functions().contains_key("or"));
        assert!(env.functions().contains_key("orValue"));
    }

    #[test]
    fn test_encoders_extension_base64() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("data", CelType::Bytes)
            .with_variable("encoded", CelType::String);

        // base64.encode(bytes) -> string
        let result = env.compile("base64.encode(data)");
        assert!(result.is_ok(), "base64.encode should compile: {:?}", result);
        assert!(result.unwrap().is_ok());

        // base64.decode(string) -> bytes
        let result = env.compile("base64.decode(encoded)");
        assert!(result.is_ok(), "base64.decode should compile: {:?}", result);
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_string_extension_char_at() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("s", CelType::String);

        let result = env.compile("s.charAt(0)");
        assert!(result.is_ok(), "charAt should compile: {:?}", result);
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_string_extension_index_of() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("s", CelType::String);

        // Basic indexOf
        let result = env.compile("s.indexOf(\"a\")");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());

        // indexOf with offset
        let result = env.compile("s.indexOf(\"a\", 2)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_string_extension_substring() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("s", CelType::String);

        // substring with one arg
        let result = env.compile("s.substring(1)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());

        // substring with two args
        let result = env.compile("s.substring(1, 5)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_string_extension_split() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("s", CelType::String);

        let result = env.compile("s.split(\",\")");
        assert!(result.is_ok());
        let check_result = result.unwrap();
        assert!(check_result.is_ok());

        // Result should be list<string>
        // The root expr type should be list<string>
    }

    #[test]
    fn test_string_extension_join() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("parts", CelType::list(CelType::String));

        // join without separator
        let result = env.compile("parts.join()");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());

        // join with separator
        let result = env.compile("parts.join(\",\")");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_math_extension_greatest() {
        let env = Env::with_standard_library().with_all_extensions();

        // Binary
        let result = env.compile("math.greatest(1, 2)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());

        // Ternary
        let result = env.compile("math.greatest(1, 2, 3)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());

        // With list
        let result = env.compile("math.greatest([1, 2, 3])");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_math_extension_least() {
        let env = Env::with_standard_library().with_all_extensions();

        let result = env.compile("math.least(1, 2)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_math_extension_abs() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("x", CelType::Int);

        let result = env.compile("math.abs(x)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_math_extension_bit_operations() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("a", CelType::Int)
            .with_variable("b", CelType::Int);

        let result = env.compile("math.bitAnd(a, b)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());

        let result = env.compile("math.bitOr(a, b)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());

        let result = env.compile("math.bitNot(a)");
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_optionals_extension_of_and_none() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("x", CelType::Int);

        // optional.of(x)
        let result = env.compile("optional.of(x)");
        assert!(result.is_ok(), "optional.of should compile: {:?}", result);
        assert!(result.unwrap().is_ok());

        // optional.none()
        let result = env.compile("optional.none()");
        assert!(result.is_ok(), "optional.none should compile: {:?}", result);
        assert!(result.unwrap().is_ok());

        // optional.ofNonZeroValue(x)
        let result = env.compile("optional.ofNonZeroValue(x)");
        assert!(
            result.is_ok(),
            "optional.ofNonZeroValue should compile: {:?}",
            result
        );
        assert!(result.unwrap().is_ok());
    }

    #[test]
    fn test_cel_bind_macro() {
        let env = Env::with_standard_library().with_all_extensions();

        // cel.bind(x, 10, x + 1) should work - bind x to 10, return x + 1
        let result = env.compile("cel.bind(x, 10, x + 1)");
        assert!(result.is_ok(), "cel.bind should parse: {:?}", result);
        let check_result = result.unwrap();
        assert!(
            check_result.is_ok(),
            "cel.bind should type-check: {:?}",
            check_result.errors
        );

        // cel.bind with string
        let result = env.compile("cel.bind(msg, \"hello\", msg + msg)");
        assert!(result.is_ok(), "cel.bind with string should parse");
        let check_result = result.unwrap();
        assert!(
            check_result.is_ok(),
            "cel.bind with string should type-check: {:?}",
            check_result.errors
        );

        // Nested cel.bind
        let result = env.compile("cel.bind(x, 1, cel.bind(y, 2, x + y))");
        assert!(result.is_ok(), "nested cel.bind should parse");
        let check_result = result.unwrap();
        assert!(
            check_result.is_ok(),
            "nested cel.bind should type-check: {:?}",
            check_result.errors
        );
    }

    #[test]
    fn test_optionals_extension_methods() {
        let env = Env::with_standard_library()
            .with_all_extensions()
            .with_variable("opt", CelType::optional(CelType::Int))
            .with_variable("opt2", CelType::optional(CelType::Int));

        // opt.hasValue()
        let result = env.compile("opt.hasValue()");
        assert!(result.is_ok(), "hasValue should parse: {:?}", result);
        let check_result = result.unwrap();
        assert!(
            check_result.is_ok(),
            "hasValue should type-check: {:?}",
            check_result.errors
        );

        // opt.value()
        let result = env.compile("opt.value()");
        assert!(result.is_ok(), "value should parse: {:?}", result);
        let check_result = result.unwrap();
        assert!(
            check_result.is_ok(),
            "value should type-check: {:?}",
            check_result.errors
        );

        // opt.or(opt2)
        let result = env.compile("opt.or(opt2)");
        assert!(result.is_ok(), "or should parse: {:?}", result);
        let check_result = result.unwrap();
        assert!(
            check_result.is_ok(),
            "or should type-check: {:?}",
            check_result.errors
        );

        // opt.orValue(42)
        let result = env.compile("opt.orValue(42)");
        assert!(result.is_ok(), "orValue should parse: {:?}", result);
        let check_result = result.unwrap();
        assert!(
            check_result.is_ok(),
            "orValue should type-check: {:?}",
            check_result.errors
        );
    }
}
