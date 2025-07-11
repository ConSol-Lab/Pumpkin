use convert_case::Case;
use convert_case::Casing;

/// Get the name of the constraint or annotation from the variant. This either is converting the
/// variant name to snake case, or retrieving the value from the `#[name(...)]` attribute.
pub(crate) fn get_explicit_name(variant: &syn::Variant) -> syn::Result<String> {
    variant
        .attrs
        .iter()
        // Find the attribute with a `name` as the path.
        .find(|attr| attr.path().get_ident().is_some_and(|ident| ident == "name"))
        // Parse the arguments of the attribute to a string literal.
        .map(|attr| {
            attr.parse_args::<syn::LitStr>()
                .map(|string_lit| string_lit.value())
        })
        // If no `name` attribute exists, return the snake-case version of the variant name.
        .unwrap_or_else(|| Ok(variant.ident.to_string().to_case(Case::Snake)))
}

/// Returns the type of the arguments for the variant if the variant has exactly the following
/// shape:
///
/// ```ignore
/// #[args]
/// Variant(Type)
/// ```
pub(crate) fn get_args_type(variant: &syn::Variant) -> Option<&syn::Type> {
    let has_args_attr = variant
        .attrs
        .iter()
        .any(|attr| attr.path().get_ident().is_some_and(|ident| ident == "args"));

    if !has_args_attr {
        return None;
    }

    if variant.fields.len() != 1 {
        // If there is not exactly one argument for this variant, then it cannot be a struct
        // constraint.
        return None;
    }

    let field = variant
        .fields
        .iter()
        .next()
        .expect("there is exactly one field");

    if field.ident.is_none() {
        Some(&field.ty)
    } else {
        None
    }
}
