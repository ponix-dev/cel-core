//! Macro system for CEL parser.
//!
//! Macros in CEL are syntactic transformations that expand at parse time.
//! They transform specific call patterns (like `list.all(x, cond)`) into
//! expanded AST nodes (like `Comprehension`).
//!
//! This module provides:
//! - [`Macro`] - Definition of a single macro
//! - [`MacroRegistry`] - Collection of macros with lookup by key
//! - [`MacroExpander`] - The expansion function type
//! - [`MacroContext`] - Context passed to expanders for node creation
//!
//! # Architecture
//!
//! Macros are keyed by `name:arg_count:is_receiver` (e.g., `"all:2:true"`).
//! This allows separate definitions for different argument counts.
//! Lookup tries the exact key first, then falls back to a var-arg key.

use std::collections::HashMap;

use crate::ast::{BinaryOp, Expr, Span, Spanned, SpannedExpr, UnaryOp};

/// Indicates whether a macro is called as a global function or as a method on a receiver.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroStyle {
    /// Global function call: `macro_name(args...)`
    Global,
    /// Receiver-style method call: `receiver.macro_name(args...)`
    Receiver,
}

/// Specifies the expected argument count for a macro.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgCount {
    /// Exact number of arguments required.
    Exact(usize),
    /// Variable arguments with a minimum count.
    VarArg(usize),
}

impl ArgCount {
    /// Check if the given argument count matches this specification.
    pub fn matches(&self, count: usize) -> bool {
        match self {
            ArgCount::Exact(n) => count == *n,
            ArgCount::VarArg(min) => count >= *min,
        }
    }

    /// Get the count value (exact count or minimum for vararg).
    pub fn count(&self) -> usize {
        match self {
            ArgCount::Exact(n) => *n,
            ArgCount::VarArg(min) => *min,
        }
    }

    /// Returns true if this is a vararg specification.
    pub fn is_vararg(&self) -> bool {
        matches!(self, ArgCount::VarArg(_))
    }
}

/// Result of macro expansion.
#[derive(Debug)]
pub enum MacroExpansion {
    /// Macro was successfully expanded to this expression.
    Expanded(SpannedExpr),
    /// Macro signature matched but expansion failed (e.g., invalid arguments).
    /// The string contains an error message.
    Error(String),
}

/// Context provided to macro expanders for creating AST nodes.
///
/// This provides the necessary state for creating synthetic AST nodes
/// during macro expansion, including ID allocation and error reporting.
pub struct MacroContext<'a> {
    /// Function to allocate the next unique node ID.
    next_id_fn: &'a mut dyn FnMut() -> i64,
    /// Accumulated errors during expansion.
    errors: Vec<(String, Span)>,
    /// Function to store macro call for IDE features.
    store_macro_call_fn: Option<&'a mut dyn FnMut(i64, &Span, &SpannedExpr, &str, &[SpannedExpr])>,
}

impl<'a> MacroContext<'a> {
    /// Create a new macro context.
    pub fn new(
        next_id_fn: &'a mut dyn FnMut() -> i64,
        store_macro_call_fn: Option<&'a mut dyn FnMut(i64, &Span, &SpannedExpr, &str, &[SpannedExpr])>,
    ) -> Self {
        Self {
            next_id_fn,
            errors: Vec::new(),
            store_macro_call_fn,
        }
    }

    /// Allocate the next unique node ID.
    pub fn next_id(&mut self) -> i64 {
        (self.next_id_fn)()
    }

    /// Add an error message.
    pub fn add_error(&mut self, message: String, span: Span) {
        self.errors.push((message, span));
    }

    /// Take accumulated errors.
    pub fn take_errors(&mut self) -> Vec<(String, Span)> {
        std::mem::take(&mut self.errors)
    }

    /// Store the original macro call expression for IDE features.
    pub fn store_macro_call(
        &mut self,
        call_id: i64,
        span: &Span,
        receiver: &SpannedExpr,
        method_name: &str,
        args: &[SpannedExpr],
    ) {
        if let Some(f) = &mut self.store_macro_call_fn {
            f(call_id, span, receiver, method_name, args);
        }
    }
}

/// Type alias for macro expander functions.
///
/// # Parameters
/// - `ctx`: Macro context for ID allocation and error reporting
/// - `span`: Source span of the entire call expression
/// - `receiver`: The receiver expression for receiver-style macros, None for global macros
/// - `args`: The arguments passed to the macro
///
/// # Returns
/// - `MacroExpansion::Expanded(expr)` on successful expansion
/// - `MacroExpansion::Error(msg)` if expansion fails
pub type MacroExpander = fn(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion;

/// Definition of a single macro.
#[derive(Clone)]
pub struct Macro {
    /// The macro name (e.g., "all", "has", "map").
    pub name: &'static str,
    /// Whether this is a global or receiver-style macro.
    pub style: MacroStyle,
    /// The expected argument count.
    pub arg_count: ArgCount,
    /// The expansion function.
    pub expander: MacroExpander,
    /// Optional description for documentation/IDE features.
    pub description: Option<&'static str>,
}

impl Macro {
    /// Create a new macro definition.
    pub const fn new(
        name: &'static str,
        style: MacroStyle,
        arg_count: ArgCount,
        expander: MacroExpander,
    ) -> Self {
        Self {
            name,
            style,
            arg_count,
            expander,
            description: None,
        }
    }

    /// Create a new macro definition with a description.
    pub const fn with_description(
        name: &'static str,
        style: MacroStyle,
        arg_count: ArgCount,
        expander: MacroExpander,
        description: &'static str,
    ) -> Self {
        Self {
            name,
            style,
            arg_count,
            expander,
            description: Some(description),
        }
    }

    /// Generate the lookup key for this macro.
    pub fn key(&self) -> String {
        make_key(self.name, self.arg_count.count(), self.style == MacroStyle::Receiver)
    }
}

impl std::fmt::Debug for Macro {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Macro")
            .field("name", &self.name)
            .field("style", &self.style)
            .field("arg_count", &self.arg_count)
            .field("description", &self.description)
            .finish_non_exhaustive()
    }
}

/// Generate a lookup key for a macro.
fn make_key(name: &str, arg_count: usize, is_receiver: bool) -> String {
    format!("{}:{}:{}", name, arg_count, is_receiver)
}

/// Registry of macros with efficient lookup.
///
/// Macros are keyed by `name:arg_count:is_receiver`.
/// Lookup tries the exact key first, then falls back to a vararg key.
#[derive(Debug, Clone)]
pub struct MacroRegistry {
    /// Map from key to macro definition.
    macros: HashMap<String, Macro>,
    /// Track vararg macros by name:is_receiver for fallback lookup.
    vararg_keys: HashMap<String, usize>,
}

impl Default for MacroRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroRegistry {
    /// Create an empty macro registry.
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            vararg_keys: HashMap::new(),
        }
    }

    /// Create a registry with the standard CEL macros.
    pub fn standard() -> Self {
        let mut registry = Self::new();
        for macro_def in STANDARD_MACROS {
            registry.register(macro_def.clone());
        }
        registry
    }

    /// Register a macro in the registry.
    pub fn register(&mut self, macro_def: Macro) {
        let key = macro_def.key();

        // Track vararg macros for fallback lookup
        if macro_def.arg_count.is_vararg() {
            let vararg_key = format!("{}:{}", macro_def.name, macro_def.style == MacroStyle::Receiver);
            self.vararg_keys.insert(vararg_key, macro_def.arg_count.count());
        }

        self.macros.insert(key, macro_def);
    }

    /// Look up a macro by name, argument count, and receiver style.
    ///
    /// First tries exact match, then falls back to vararg match if applicable.
    pub fn lookup(&self, name: &str, arg_count: usize, is_receiver: bool) -> Option<&Macro> {
        // Try exact match first
        let exact_key = make_key(name, arg_count, is_receiver);
        if let Some(m) = self.macros.get(&exact_key) {
            return Some(m);
        }

        // Try vararg fallback
        let vararg_lookup_key = format!("{}:{}", name, is_receiver);
        if let Some(&min_args) = self.vararg_keys.get(&vararg_lookup_key) {
            if arg_count >= min_args {
                let vararg_key = make_key(name, min_args, is_receiver);
                return self.macros.get(&vararg_key);
            }
        }

        None
    }

    /// Check if the registry contains a macro with the given name.
    pub fn contains(&self, name: &str) -> bool {
        self.macros.values().any(|m| m.name == name)
    }

    /// Get an iterator over all registered macros.
    pub fn iter(&self) -> impl Iterator<Item = &Macro> {
        self.macros.values()
    }

    /// Get the number of registered macros.
    pub fn len(&self) -> usize {
        self.macros.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.macros.is_empty()
    }
}

// ============================================================================
// Standard CEL Macros
// ============================================================================

/// Accumulator variable name used in comprehension expansions.
const ACCU_VAR: &str = "__result__";

/// Standard CEL macros.
pub static STANDARD_MACROS: &[Macro] = &[
    // has(m.x) - global, 1 arg
    Macro::with_description(
        "has",
        MacroStyle::Global,
        ArgCount::Exact(1),
        expand_has,
        "Tests whether a field is set on a message",
    ),

    // all - receiver, 2 or 3 args
    Macro::with_description(
        "all",
        MacroStyle::Receiver,
        ArgCount::Exact(2),
        expand_all_2arg,
        "Tests whether all elements satisfy a condition",
    ),
    Macro::with_description(
        "all",
        MacroStyle::Receiver,
        ArgCount::Exact(3),
        expand_all_3arg,
        "Tests whether all elements satisfy a condition (two-variable form)",
    ),

    // exists - receiver, 2 or 3 args
    Macro::with_description(
        "exists",
        MacroStyle::Receiver,
        ArgCount::Exact(2),
        expand_exists_2arg,
        "Tests whether any element satisfies a condition",
    ),
    Macro::with_description(
        "exists",
        MacroStyle::Receiver,
        ArgCount::Exact(3),
        expand_exists_3arg,
        "Tests whether any element satisfies a condition (two-variable form)",
    ),

    // exists_one - receiver, 2 or 3 args
    Macro::with_description(
        "exists_one",
        MacroStyle::Receiver,
        ArgCount::Exact(2),
        expand_exists_one_2arg,
        "Tests whether exactly one element satisfies a condition",
    ),
    Macro::with_description(
        "exists_one",
        MacroStyle::Receiver,
        ArgCount::Exact(3),
        expand_exists_one_3arg,
        "Tests whether exactly one element satisfies a condition (two-variable form)",
    ),

    // map - receiver, 2 or 3 args
    Macro::with_description(
        "map",
        MacroStyle::Receiver,
        ArgCount::Exact(2),
        expand_map_2arg,
        "Transforms elements of a list",
    ),
    Macro::with_description(
        "map",
        MacroStyle::Receiver,
        ArgCount::Exact(3),
        expand_map_3arg,
        "Transforms elements of a list with filtering",
    ),

    // filter - receiver, 2 args
    Macro::with_description(
        "filter",
        MacroStyle::Receiver,
        ArgCount::Exact(2),
        expand_filter,
        "Filters elements of a list by a condition",
    ),

    // transformList - receiver, 3 or 4 args
    Macro::with_description(
        "transformList",
        MacroStyle::Receiver,
        ArgCount::Exact(3),
        expand_transform_list_3arg,
        "Transforms list elements with index and value variables",
    ),
    Macro::with_description(
        "transformList",
        MacroStyle::Receiver,
        ArgCount::Exact(4),
        expand_transform_list_4arg,
        "Transforms list elements with index, value, and filter",
    ),

    // transformMap - receiver, 3 or 4 args
    Macro::with_description(
        "transformMap",
        MacroStyle::Receiver,
        ArgCount::Exact(3),
        expand_transform_map_3arg,
        "Transforms map entries with key and value variables",
    ),
    Macro::with_description(
        "transformMap",
        MacroStyle::Receiver,
        ArgCount::Exact(4),
        expand_transform_map_4arg,
        "Transforms map entries with key, value, and filter",
    ),
];

// === Helper Functions ===

/// Create a synthetic spanned expression.
fn synthetic(ctx: &mut MacroContext, node: Expr, span: Span) -> SpannedExpr {
    Spanned::new(ctx.next_id(), node, span)
}

/// Extract iteration variable name from an expression.
/// Returns None and adds error if not a simple identifier.
fn extract_iter_var(ctx: &mut MacroContext, expr: &SpannedExpr) -> Option<String> {
    match &expr.node {
        Expr::Ident(name) => Some(name.clone()),
        _ => {
            ctx.add_error(
                "iteration variable must be an identifier".to_string(),
                expr.span.clone(),
            );
            None
        }
    }
}

// === has() Macro ===

/// Expand `has(m.x)` to `MemberTestOnly { expr: m, field: x }`.
fn expand_has(
    ctx: &mut MacroContext,
    span: Span,
    _receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    if args.len() != 1 {
        return MacroExpansion::Error(format!("has() requires 1 argument, got {}", args.len()));
    }

    let arg = args.into_iter().next().unwrap();

    match arg.node {
        Expr::Member { expr, field } => {
            let result = Spanned::new(
                ctx.next_id(),
                Expr::MemberTestOnly { expr, field },
                span,
            );
            MacroExpansion::Expanded(result)
        }
        _ => MacroExpansion::Error(
            "has() argument must be a field selection (e.g., has(m.x))".to_string(),
        ),
    }
}

// === all() Macro ===

fn expand_all_2arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("all() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let cond = args[1].clone();

    expand_all_impl(ctx, span, receiver, iter_var, String::new(), cond, &args)
}

fn expand_all_3arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("all() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let iter_var2 = match extract_iter_var(ctx, &args[1]) {
        Some(v) => v,
        None => return MacroExpansion::Error("second argument must be an identifier".to_string()),
    };
    let cond = args[2].clone();

    expand_all_impl(ctx, span, receiver, iter_var, iter_var2, cond, &args)
}

fn expand_all_impl(
    ctx: &mut MacroContext,
    span: Span,
    receiver: SpannedExpr,
    iter_var: String,
    iter_var2: String,
    cond: SpannedExpr,
    args: &[SpannedExpr],
) -> MacroExpansion {
    let call_id = ctx.next_id();
    ctx.store_macro_call(call_id, &span, &receiver, "all", args);

    let accu_var = ACCU_VAR.to_string();
    let accu_init = synthetic(ctx, Expr::Bool(true), span.clone());
    let loop_condition = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());

    let accu_ref = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let loop_step = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::And,
            left: Box::new(cond),
            right: Box::new(accu_ref),
        },
        span.clone(),
    );

    let result = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());

    MacroExpansion::Expanded(Spanned::new(
        call_id,
        Expr::Comprehension {
            iter_var,
            iter_var2,
            iter_range: Box::new(receiver),
            accu_var,
            accu_init: Box::new(accu_init),
            loop_condition: Box::new(loop_condition),
            loop_step: Box::new(loop_step),
            result: Box::new(result),
        },
        span,
    ))
}

// === exists() Macro ===

fn expand_exists_2arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("exists() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let cond = args[1].clone();

    expand_exists_impl(ctx, span, receiver, iter_var, String::new(), cond, &args)
}

fn expand_exists_3arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("exists() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let iter_var2 = match extract_iter_var(ctx, &args[1]) {
        Some(v) => v,
        None => return MacroExpansion::Error("second argument must be an identifier".to_string()),
    };
    let cond = args[2].clone();

    expand_exists_impl(ctx, span, receiver, iter_var, iter_var2, cond, &args)
}

fn expand_exists_impl(
    ctx: &mut MacroContext,
    span: Span,
    receiver: SpannedExpr,
    iter_var: String,
    iter_var2: String,
    cond: SpannedExpr,
    args: &[SpannedExpr],
) -> MacroExpansion {
    let call_id = ctx.next_id();
    ctx.store_macro_call(call_id, &span, &receiver, "exists", args);

    let accu_var = ACCU_VAR.to_string();
    let accu_init = synthetic(ctx, Expr::Bool(false), span.clone());

    let accu_ref_cond = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let loop_condition = synthetic(
        ctx,
        Expr::Unary {
            op: UnaryOp::Not,
            expr: Box::new(accu_ref_cond),
        },
        span.clone(),
    );

    let accu_ref_step = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let loop_step = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Or,
            left: Box::new(cond),
            right: Box::new(accu_ref_step),
        },
        span.clone(),
    );

    let result = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());

    MacroExpansion::Expanded(Spanned::new(
        call_id,
        Expr::Comprehension {
            iter_var,
            iter_var2,
            iter_range: Box::new(receiver),
            accu_var,
            accu_init: Box::new(accu_init),
            loop_condition: Box::new(loop_condition),
            loop_step: Box::new(loop_step),
            result: Box::new(result),
        },
        span,
    ))
}

// === exists_one() Macro ===

fn expand_exists_one_2arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("exists_one() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let cond = args[1].clone();

    expand_exists_one_impl(ctx, span, receiver, iter_var, String::new(), cond, &args)
}

fn expand_exists_one_3arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("exists_one() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let iter_var2 = match extract_iter_var(ctx, &args[1]) {
        Some(v) => v,
        None => return MacroExpansion::Error("second argument must be an identifier".to_string()),
    };
    let cond = args[2].clone();

    expand_exists_one_impl(ctx, span, receiver, iter_var, iter_var2, cond, &args)
}

fn expand_exists_one_impl(
    ctx: &mut MacroContext,
    span: Span,
    receiver: SpannedExpr,
    iter_var: String,
    iter_var2: String,
    cond: SpannedExpr,
    args: &[SpannedExpr],
) -> MacroExpansion {
    let call_id = ctx.next_id();
    ctx.store_macro_call(call_id, &span, &receiver, "exists_one", args);

    let accu_var = ACCU_VAR.to_string();
    let accu_init = synthetic(ctx, Expr::Int(0), span.clone());

    let accu_ref_cond = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let one_cond = synthetic(ctx, Expr::Int(1), span.clone());
    let loop_condition = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Le,
            left: Box::new(accu_ref_cond),
            right: Box::new(one_cond),
        },
        span.clone(),
    );

    let accu_ref_then = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let one_step = synthetic(ctx, Expr::Int(1), span.clone());
    let increment = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(accu_ref_then),
            right: Box::new(one_step),
        },
        span.clone(),
    );
    let accu_ref_else = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let loop_step = synthetic(
        ctx,
        Expr::Ternary {
            cond: Box::new(cond),
            then_expr: Box::new(increment),
            else_expr: Box::new(accu_ref_else),
        },
        span.clone(),
    );

    let accu_ref_result = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let one_result = synthetic(ctx, Expr::Int(1), span.clone());
    let result = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Eq,
            left: Box::new(accu_ref_result),
            right: Box::new(one_result),
        },
        span.clone(),
    );

    MacroExpansion::Expanded(Spanned::new(
        call_id,
        Expr::Comprehension {
            iter_var,
            iter_var2,
            iter_range: Box::new(receiver),
            accu_var,
            accu_init: Box::new(accu_init),
            loop_condition: Box::new(loop_condition),
            loop_step: Box::new(loop_step),
            result: Box::new(result),
        },
        span,
    ))
}

// === map() Macro ===

fn expand_map_2arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("map() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let transform = args[1].clone();

    expand_map_impl(ctx, span, receiver, iter_var, None, transform, &args)
}

fn expand_map_3arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("map() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let filter = args[1].clone();
    let transform = args[2].clone();

    expand_map_impl(ctx, span, receiver, iter_var, Some(filter), transform, &args)
}

fn expand_map_impl(
    ctx: &mut MacroContext,
    span: Span,
    receiver: SpannedExpr,
    iter_var: String,
    filter_cond: Option<SpannedExpr>,
    transform: SpannedExpr,
    args: &[SpannedExpr],
) -> MacroExpansion {
    let call_id = ctx.next_id();
    ctx.store_macro_call(call_id, &span, &receiver, "map", args);

    let accu_var = ACCU_VAR.to_string();
    let accu_init = synthetic(ctx, Expr::List(vec![]), span.clone());
    let loop_condition = synthetic(ctx, Expr::Bool(true), span.clone());

    let transformed_list = synthetic(ctx, Expr::List(vec![transform]), span.clone());
    let accu_ref_step = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let append_step = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(accu_ref_step),
            right: Box::new(transformed_list),
        },
        span.clone(),
    );

    let loop_step = if let Some(filter) = filter_cond {
        let accu_ref_else = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
        synthetic(
            ctx,
            Expr::Ternary {
                cond: Box::new(filter),
                then_expr: Box::new(append_step),
                else_expr: Box::new(accu_ref_else),
            },
            span.clone(),
        )
    } else {
        append_step
    };

    let result = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());

    MacroExpansion::Expanded(Spanned::new(
        call_id,
        Expr::Comprehension {
            iter_var,
            iter_var2: String::new(),
            iter_range: Box::new(receiver),
            accu_var,
            accu_init: Box::new(accu_init),
            loop_condition: Box::new(loop_condition),
            loop_step: Box::new(loop_step),
            result: Box::new(result),
        },
        span,
    ))
}

// === filter() Macro ===

fn expand_filter(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("filter() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let cond = args[1].clone();

    let call_id = ctx.next_id();
    ctx.store_macro_call(call_id, &span, &receiver, "filter", &args);

    let accu_var = ACCU_VAR.to_string();
    let accu_init = synthetic(ctx, Expr::List(vec![]), span.clone());
    let loop_condition = synthetic(ctx, Expr::Bool(true), span.clone());

    let iter_ref = synthetic(ctx, Expr::Ident(iter_var.clone()), span.clone());
    let element_list = synthetic(ctx, Expr::List(vec![iter_ref]), span.clone());

    let accu_ref_then = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let append_step = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(accu_ref_then),
            right: Box::new(element_list),
        },
        span.clone(),
    );

    let accu_ref_else = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let loop_step = synthetic(
        ctx,
        Expr::Ternary {
            cond: Box::new(cond),
            then_expr: Box::new(append_step),
            else_expr: Box::new(accu_ref_else),
        },
        span.clone(),
    );

    let result = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());

    MacroExpansion::Expanded(Spanned::new(
        call_id,
        Expr::Comprehension {
            iter_var,
            iter_var2: String::new(),
            iter_range: Box::new(receiver),
            accu_var,
            accu_init: Box::new(accu_init),
            loop_condition: Box::new(loop_condition),
            loop_step: Box::new(loop_step),
            result: Box::new(result),
        },
        span,
    ))
}

// === transformList() Macro ===

fn expand_transform_list_3arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("transformList() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let iter_var2 = match extract_iter_var(ctx, &args[1]) {
        Some(v) => v,
        None => return MacroExpansion::Error("second argument must be an identifier".to_string()),
    };
    let transform = args[2].clone();

    expand_transform_list_impl(ctx, span, receiver, iter_var, iter_var2, None, transform, &args)
}

fn expand_transform_list_4arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("transformList() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let iter_var2 = match extract_iter_var(ctx, &args[1]) {
        Some(v) => v,
        None => return MacroExpansion::Error("second argument must be an identifier".to_string()),
    };
    let filter = args[2].clone();
    let transform = args[3].clone();

    expand_transform_list_impl(ctx, span, receiver, iter_var, iter_var2, Some(filter), transform, &args)
}

fn expand_transform_list_impl(
    ctx: &mut MacroContext,
    span: Span,
    receiver: SpannedExpr,
    iter_var: String,
    iter_var2: String,
    filter_cond: Option<SpannedExpr>,
    transform: SpannedExpr,
    args: &[SpannedExpr],
) -> MacroExpansion {
    let call_id = ctx.next_id();
    ctx.store_macro_call(call_id, &span, &receiver, "transformList", args);

    let accu_var = ACCU_VAR.to_string();
    let accu_init = synthetic(ctx, Expr::List(vec![]), span.clone());
    let loop_condition = synthetic(ctx, Expr::Bool(true), span.clone());

    let transformed_list = synthetic(ctx, Expr::List(vec![transform]), span.clone());
    let accu_ref_step = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let append_step = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(accu_ref_step),
            right: Box::new(transformed_list),
        },
        span.clone(),
    );

    let loop_step = if let Some(filter) = filter_cond {
        let accu_ref_else = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
        synthetic(
            ctx,
            Expr::Ternary {
                cond: Box::new(filter),
                then_expr: Box::new(append_step),
                else_expr: Box::new(accu_ref_else),
            },
            span.clone(),
        )
    } else {
        append_step
    };

    let result = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());

    MacroExpansion::Expanded(Spanned::new(
        call_id,
        Expr::Comprehension {
            iter_var,
            iter_var2,
            iter_range: Box::new(receiver),
            accu_var,
            accu_init: Box::new(accu_init),
            loop_condition: Box::new(loop_condition),
            loop_step: Box::new(loop_step),
            result: Box::new(result),
        },
        span,
    ))
}

// === transformMap() Macro ===

fn expand_transform_map_3arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("transformMap() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let iter_var2 = match extract_iter_var(ctx, &args[1]) {
        Some(v) => v,
        None => return MacroExpansion::Error("second argument must be an identifier".to_string()),
    };
    let transform = args[2].clone();

    expand_transform_map_impl(ctx, span, receiver, iter_var, iter_var2, None, transform, &args)
}

fn expand_transform_map_4arg(
    ctx: &mut MacroContext,
    span: Span,
    receiver: Option<SpannedExpr>,
    args: Vec<SpannedExpr>,
) -> MacroExpansion {
    let receiver = match receiver {
        Some(r) => r,
        None => return MacroExpansion::Error("transformMap() requires a receiver".to_string()),
    };

    let iter_var = match extract_iter_var(ctx, &args[0]) {
        Some(v) => v,
        None => return MacroExpansion::Error("first argument must be an identifier".to_string()),
    };
    let iter_var2 = match extract_iter_var(ctx, &args[1]) {
        Some(v) => v,
        None => return MacroExpansion::Error("second argument must be an identifier".to_string()),
    };
    let filter = args[2].clone();
    let transform = args[3].clone();

    expand_transform_map_impl(ctx, span, receiver, iter_var, iter_var2, Some(filter), transform, &args)
}

fn expand_transform_map_impl(
    ctx: &mut MacroContext,
    span: Span,
    receiver: SpannedExpr,
    iter_var: String,
    iter_var2: String,
    filter_cond: Option<SpannedExpr>,
    transform: SpannedExpr,
    args: &[SpannedExpr],
) -> MacroExpansion {
    let call_id = ctx.next_id();
    ctx.store_macro_call(call_id, &span, &receiver, "transformMap", args);

    let accu_var = ACCU_VAR.to_string();
    let accu_init = synthetic(ctx, Expr::Map(vec![]), span.clone());
    let loop_condition = synthetic(ctx, Expr::Bool(true), span.clone());

    let key_ref = synthetic(ctx, Expr::Ident(iter_var.clone()), span.clone());
    let transformed_map = synthetic(ctx, Expr::Map(vec![(key_ref, transform)]), span.clone());
    let accu_ref_step = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
    let append_step = synthetic(
        ctx,
        Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(accu_ref_step),
            right: Box::new(transformed_map),
        },
        span.clone(),
    );

    let loop_step = if let Some(filter) = filter_cond {
        let accu_ref_else = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());
        synthetic(
            ctx,
            Expr::Ternary {
                cond: Box::new(filter),
                then_expr: Box::new(append_step),
                else_expr: Box::new(accu_ref_else),
            },
            span.clone(),
        )
    } else {
        append_step
    };

    let result = synthetic(ctx, Expr::Ident(accu_var.clone()), span.clone());

    MacroExpansion::Expanded(Spanned::new(
        call_id,
        Expr::Comprehension {
            iter_var,
            iter_var2,
            iter_range: Box::new(receiver),
            accu_var,
            accu_init: Box::new(accu_init),
            loop_condition: Box::new(loop_condition),
            loop_step: Box::new(loop_step),
            result: Box::new(result),
        },
        span,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_expander(
        _ctx: &mut MacroContext,
        _span: Span,
        _receiver: Option<SpannedExpr>,
        _args: Vec<SpannedExpr>,
    ) -> MacroExpansion {
        MacroExpansion::Error("dummy".to_string())
    }

    #[test]
    fn test_arg_count_exact() {
        let exact = ArgCount::Exact(2);
        assert!(exact.matches(2));
        assert!(!exact.matches(1));
        assert!(!exact.matches(3));
        assert_eq!(exact.count(), 2);
        assert!(!exact.is_vararg());
    }

    #[test]
    fn test_arg_count_vararg() {
        let vararg = ArgCount::VarArg(2);
        assert!(vararg.matches(2));
        assert!(vararg.matches(3));
        assert!(vararg.matches(10));
        assert!(!vararg.matches(1));
        assert_eq!(vararg.count(), 2);
        assert!(vararg.is_vararg());
    }

    #[test]
    fn test_macro_key() {
        let m = Macro::new("all", MacroStyle::Receiver, ArgCount::Exact(2), dummy_expander);
        assert_eq!(m.key(), "all:2:true");

        let m2 = Macro::new("has", MacroStyle::Global, ArgCount::Exact(1), dummy_expander);
        assert_eq!(m2.key(), "has:1:false");
    }

    #[test]
    fn test_registry_lookup_exact() {
        let mut registry = MacroRegistry::new();
        registry.register(Macro::new("all", MacroStyle::Receiver, ArgCount::Exact(2), dummy_expander));
        registry.register(Macro::new("all", MacroStyle::Receiver, ArgCount::Exact(3), dummy_expander));

        assert!(registry.lookup("all", 2, true).is_some());
        assert!(registry.lookup("all", 3, true).is_some());
        assert!(registry.lookup("all", 4, true).is_none());
        assert!(registry.lookup("all", 2, false).is_none());
    }

    #[test]
    fn test_registry_lookup_vararg() {
        let mut registry = MacroRegistry::new();
        registry.register(Macro::new("custom", MacroStyle::Receiver, ArgCount::VarArg(2), dummy_expander));

        assert!(registry.lookup("custom", 2, true).is_some());
        assert!(registry.lookup("custom", 3, true).is_some());
        assert!(registry.lookup("custom", 10, true).is_some());
        assert!(registry.lookup("custom", 1, true).is_none());
    }

    #[test]
    fn test_registry_standard() {
        let registry = MacroRegistry::standard();

        assert!(registry.lookup("has", 1, false).is_some());
        assert!(registry.lookup("all", 2, true).is_some());
        assert!(registry.lookup("all", 3, true).is_some());
        assert!(registry.lookup("exists", 2, true).is_some());
        assert!(registry.lookup("exists", 3, true).is_some());
        assert!(registry.lookup("exists_one", 2, true).is_some());
        assert!(registry.lookup("exists_one", 3, true).is_some());
        assert!(registry.lookup("map", 2, true).is_some());
        assert!(registry.lookup("map", 3, true).is_some());
        assert!(registry.lookup("filter", 2, true).is_some());
    }

    #[test]
    fn test_registry_contains() {
        let registry = MacroRegistry::standard();
        assert!(registry.contains("has"));
        assert!(registry.contains("all"));
        assert!(!registry.contains("nonexistent"));
    }
}
