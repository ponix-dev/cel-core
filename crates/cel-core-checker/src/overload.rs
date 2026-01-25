//! Overload resolution for CEL function calls.
//!
//! This module implements the algorithm for selecting matching function overloads
//! based on argument types, following cel-go's approach.

use std::collections::HashMap;
use std::sync::Arc;

use cel_core_common::CelType;

use crate::decls::FunctionDecl;

/// Result of overload resolution.
#[derive(Debug)]
pub struct OverloadResult {
    /// The resolved result type.
    pub result_type: CelType,
    /// The matching overload IDs (may be multiple if resolution is ambiguous).
    pub overload_ids: Vec<String>,
}

/// Resolve the best matching overload(s) for a function call.
///
/// # Arguments
///
/// * `func` - The function declaration with all overloads
/// * `target` - The receiver type for method calls (None for standalone calls)
/// * `args` - The argument types
/// * `substitutions` - Mutable map for collecting type parameter bindings
///
/// # Returns
///
/// The overload resolution result, or None if no overloads match.
pub fn resolve_overload(
    func: &FunctionDecl,
    target: Option<&CelType>,
    args: &[CelType],
    substitutions: &mut HashMap<Arc<str>, CelType>,
) -> Option<OverloadResult> {
    let mut matching_overloads = Vec::new();
    let mut result_type = None;

    for overload in &func.overloads {
        // Skip if call style doesn't match
        if target.is_some() != overload.is_member {
            continue;
        }

        // Build full argument list (receiver + args for methods)
        let full_args: Vec<&CelType> = if let Some(recv) = target {
            std::iter::once(recv).chain(args.iter()).collect()
        } else {
            args.iter().collect()
        };

        // Check if argument count matches
        if full_args.len() != overload.params.len() {
            continue;
        }

        // Try to match this overload
        let mut local_subs = substitutions.clone();
        if try_match_overload(&full_args, &overload.params, &overload.type_params, &mut local_subs) {
            matching_overloads.push(overload);

            // Compute result type with substitutions
            let res = substitute_type(&overload.result, &local_subs);
            result_type = Some(res);

            // Update substitutions
            *substitutions = local_subs;
        }
    }

    if matching_overloads.is_empty() {
        return None;
    }

    // Collect overload IDs
    let overload_ids: Vec<String> = matching_overloads.iter().map(|o| o.id.clone()).collect();

    Some(OverloadResult {
        result_type: result_type.unwrap_or(CelType::Dyn),
        overload_ids,
    })
}

/// Try to match argument types against parameter types, binding type parameters.
fn try_match_overload(
    args: &[&CelType],
    params: &[CelType],
    type_params: &[String],
    substitutions: &mut HashMap<Arc<str>, CelType>,
) -> bool {
    // Initialize fresh type variables for type parameters
    for param_name in type_params {
        if !substitutions.contains_key(param_name.as_str()) {
            substitutions.insert(Arc::from(param_name.as_str()), CelType::fresh_type_var());
        }
    }

    // Check each argument against its parameter
    for (arg, param) in args.iter().zip(params.iter()) {
        let substituted_param = substitute_type(param, substitutions);
        if !is_assignable(arg, &substituted_param, substitutions) {
            return false;
        }
    }

    true
}

/// Check if an argument type is assignable to a parameter type.
fn is_assignable(
    arg: &CelType,
    param: &CelType,
    substitutions: &mut HashMap<Arc<str>, CelType>,
) -> bool {
    // Type parameters bind to argument types
    if let CelType::TypeParam(name) = param {
        if let Some(bound) = substitutions.get(name).cloned() {
            // Already bound - check compatibility
            return is_types_compatible(&bound, arg, substitutions);
        } else {
            // Bind the type parameter
            substitutions.insert(name.clone(), arg.clone());
            return true;
        }
    }

    // Type variables unify with anything
    if matches!(param, CelType::TypeVar(_)) || matches!(arg, CelType::TypeVar(_)) {
        return true;
    }

    // Dyn is compatible with everything
    if matches!(arg, CelType::Dyn) || matches!(param, CelType::Dyn) {
        return true;
    }

    // Error is compatible with everything (for error recovery)
    if matches!(arg, CelType::Error) || matches!(param, CelType::Error) {
        return true;
    }

    // Same type is always assignable
    if arg == param {
        return true;
    }

    // Check structural compatibility
    match (arg, param) {
        (CelType::List(arg_elem), CelType::List(param_elem)) => {
            is_assignable(arg_elem, param_elem, substitutions)
        }
        (CelType::Map(arg_key, arg_val), CelType::Map(param_key, param_val)) => {
            is_assignable(arg_key, param_key, substitutions)
                && is_assignable(arg_val, param_val, substitutions)
        }
        (CelType::Type(arg_inner), CelType::Type(param_inner)) => {
            is_assignable(arg_inner, param_inner, substitutions)
        }
        (CelType::Wrapper(arg_inner), CelType::Wrapper(param_inner)) => {
            is_assignable(arg_inner, param_inner, substitutions)
        }
        // Null is assignable to wrapper types
        (CelType::Null, CelType::Wrapper(_)) => true,
        // Underlying type is assignable to wrapper (boxing)
        (inner, CelType::Wrapper(param_inner)) => is_assignable(inner, param_inner, substitutions),
        // Wrapper is assignable to underlying type (unboxing)
        (CelType::Wrapper(arg_inner), inner) => is_assignable(arg_inner.as_ref(), inner, substitutions),
        _ => false,
    }
}

/// Check if two types are compatible (for checking bound type parameters).
fn is_types_compatible(
    bound: &CelType,
    arg: &CelType,
    substitutions: &mut HashMap<Arc<str>, CelType>,
) -> bool {
    // Type variables are compatible with anything
    if matches!(bound, CelType::TypeVar(_)) {
        return true;
    }

    // Try assignability in both directions for type parameter resolution
    is_assignable(arg, bound, substitutions) || is_assignable(bound, arg, &mut HashMap::new())
}

/// Substitute type parameters in a type with their bound values.
pub fn substitute_type(ty: &CelType, substitutions: &HashMap<Arc<str>, CelType>) -> CelType {
    match ty {
        CelType::TypeParam(name) => {
            if let Some(bound) = substitutions.get(name) {
                // Recursively substitute in case the bound type also has parameters
                substitute_type(bound, substitutions)
            } else {
                ty.clone()
            }
        }
        CelType::List(elem) => CelType::list(substitute_type(elem, substitutions)),
        CelType::Map(key, val) => CelType::map(
            substitute_type(key, substitutions),
            substitute_type(val, substitutions),
        ),
        CelType::Type(inner) => CelType::type_of(substitute_type(inner, substitutions)),
        CelType::Wrapper(inner) => CelType::wrapper(substitute_type(inner, substitutions)),
        CelType::Function { params, result } => CelType::Function {
            params: Arc::from(
                params
                    .iter()
                    .map(|p| substitute_type(p, substitutions))
                    .collect::<Vec<_>>(),
            ),
            result: Arc::new(substitute_type(result, substitutions)),
        },
        CelType::Abstract { name, params: abs_params } => CelType::Abstract {
            name: name.clone(),
            params: Arc::from(
                abs_params
                    .iter()
                    .map(|p| substitute_type(p, substitutions))
                    .collect::<Vec<_>>(),
            ),
        },
        // Other types don't have type parameters
        _ => ty.clone(),
    }
}

/// Finalize type by replacing unbound type variables with Dyn.
pub fn finalize_type(ty: &CelType) -> CelType {
    match ty {
        CelType::TypeVar(_) => CelType::Dyn,
        CelType::TypeParam(_) => CelType::Dyn,
        CelType::List(elem) => CelType::list(finalize_type(elem)),
        CelType::Map(key, val) => CelType::map(finalize_type(key), finalize_type(val)),
        CelType::Type(inner) => CelType::type_of(finalize_type(inner)),
        CelType::Wrapper(inner) => CelType::wrapper(finalize_type(inner)),
        CelType::Function { params, result } => CelType::Function {
            params: Arc::from(
                params.iter().map(finalize_type).collect::<Vec<_>>(),
            ),
            result: Arc::new(finalize_type(result)),
        },
        CelType::Abstract { name, params: abs_params } => CelType::Abstract {
            name: name.clone(),
            params: Arc::from(
                abs_params.iter().map(finalize_type).collect::<Vec<_>>(),
            ),
        },
        _ => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decls::OverloadDecl;

    #[test]
    fn test_resolve_simple_overload() {
        let func = FunctionDecl::new("_+_")
            .with_overload(OverloadDecl::function(
                "add_int64_int64",
                vec![CelType::Int, CelType::Int],
                CelType::Int,
            ))
            .with_overload(OverloadDecl::function(
                "add_string_string",
                vec![CelType::String, CelType::String],
                CelType::String,
            ));

        let mut subs = HashMap::new();
        let result = resolve_overload(&func, None, &[CelType::Int, CelType::Int], &mut subs);

        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.result_type, CelType::Int);
        assert!(result.overload_ids.contains(&"add_int64_int64".to_string()));
    }

    #[test]
    fn test_resolve_method_overload() {
        let func = FunctionDecl::new("contains")
            .with_overload(OverloadDecl::method(
                "string_contains_string",
                vec![CelType::String, CelType::String],
                CelType::Bool,
            ));

        let mut subs = HashMap::new();
        let result = resolve_overload(&func, Some(&CelType::String), &[CelType::String], &mut subs);

        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.result_type, CelType::Bool);
    }

    #[test]
    fn test_resolve_generic_overload() {
        let func = FunctionDecl::new("_==_")
            .with_overload(
                OverloadDecl::function(
                    "equals",
                    vec![CelType::type_param("T"), CelType::type_param("T")],
                    CelType::Bool,
                )
                .with_type_params(vec!["T".to_string()]),
            );

        let mut subs = HashMap::new();
        let result = resolve_overload(&func, None, &[CelType::Int, CelType::Int], &mut subs);

        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.result_type, CelType::Bool);
    }

    #[test]
    fn test_resolve_no_match() {
        let func = FunctionDecl::new("_+_")
            .with_overload(OverloadDecl::function(
                "add_int64_int64",
                vec![CelType::Int, CelType::Int],
                CelType::Int,
            ));

        let mut subs = HashMap::new();
        let result = resolve_overload(&func, None, &[CelType::String, CelType::Int], &mut subs);

        assert!(result.is_none());
    }

    #[test]
    fn test_substitute_type() {
        let mut subs = HashMap::new();
        subs.insert(Arc::from("T"), CelType::Int);

        let list_t = CelType::list(CelType::type_param("T"));
        let result = substitute_type(&list_t, &subs);

        assert_eq!(result, CelType::list(CelType::Int));
    }

    #[test]
    fn test_finalize_type() {
        let list_var = CelType::list(CelType::fresh_type_var());
        let result = finalize_type(&list_var);

        assert_eq!(result, CelType::list(CelType::Dyn));
    }

    #[test]
    fn test_dyn_compatible() {
        let mut subs = HashMap::new();
        assert!(is_assignable(&CelType::Dyn, &CelType::Int, &mut subs));
        assert!(is_assignable(&CelType::Int, &CelType::Dyn, &mut subs));
    }
}
