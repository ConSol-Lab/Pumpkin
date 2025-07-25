use quote::quote;

/// Construct a token stream that initialises a value with name `value_type` and the arguments
/// described in `fields`.
pub(crate) fn initialise_value(
    value_type: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    // For every field, initialise the value for that field.
    let field_values = fields.iter().enumerate().map(|(idx, field)| {
        let ty = &field.ty;

        // If the field has a name, then prepend the field value with the name: `name: value`.
        let value_prefix = if let Some(ident) = &field.ident {
            quote! { #ident: }
        } else {
            quote! {}
        };

        // If there is an `#[annotation]` attribute on the field, then the value is the result of
        // parsing a nested annotation. Otherwise, we look at the type of the field
        // and parse the value corresponding to that type.
        if field.attrs.iter().any(|attr| {
            attr.path()
                .get_ident()
                .is_some_and(|ident| ident == "annotation")
        }) {
            quote! {
                #value_prefix <#ty as ::fzn_rs::FromNestedAnnotation>::from_argument(
                    &arguments[#idx],
                )?
            }
        } else {
            quote! {
                #value_prefix <#ty as ::fzn_rs::FromAnnotationArgument>::from_argument(
                    &arguments[#idx],
                )?
            }
        }
    });

    // Complete the value initialiser by prepending the type name to the field values.
    let value_initialiser = match fields {
        syn::Fields::Named(_) => quote! { #value_type { #(#field_values),* } },
        syn::Fields::Unnamed(_) => quote! { #value_type ( #(#field_values),* ) },
        syn::Fields::Unit => quote! { #value_type },
    };

    let num_arguments = fields.len();

    // Output the final initialisation, with checking of number of arguments.
    quote! {
        if arguments.len() != #num_arguments {
            return Err(::fzn_rs::InstanceError::IncorrectNumberOfArguments {
                expected: #num_arguments,
                actual: arguments.len(),
            });
        }

        Ok(Some(#value_initialiser))
    }
}

/// Create the parsing code for one annotation corresponding to the given variant.
pub(crate) fn variant_to_annotation(variant: &syn::Variant) -> proc_macro2::TokenStream {
    // Determine the flatzinc annotation name.
    let name = match crate::common::get_explicit_name(variant) {
        Ok(name) => name,
        Err(_) => {
            return quote! {
                compile_error!("Invalid usage of #[name(...)]");
            }
        }
    };

    let variant_name = &variant.ident;

    // If variant argument is a struct, then delegate parsing of the annotation arguments to that
    // struct.
    if let Some(constraint_type) = crate::common::get_args_type(variant) {
        return quote! {
            ::fzn_rs::ast::Annotation::Call(::fzn_rs::ast::AnnotationCall {
                name,
                arguments,
            }) if name.as_ref() == #name => {
                let args = <#constraint_type as ::fzn_rs::FlatZincAnnotation>::from_ast_required(annotation)?;
                let value = #variant_name(args);
                Ok(Some(value))
            }
        };
    }

    // If the variant has no arguments, parse an atom annotaton. Otherwise, initialise the values
    // of the variant arguments.
    if matches!(variant.fields, syn::Fields::Unit) {
        quote! {
            ::fzn_rs::ast::Annotation::Atom(ident) if ident.as_ref() == #name => {
                Ok(Some(#variant_name))
            }
        }
    } else {
        let value = initialise_value(&variant.ident, &variant.fields);

        quote! {
            ::fzn_rs::ast::Annotation::Call(::fzn_rs::ast::AnnotationCall {
                name,
                arguments,
            }) if name.as_ref() == #name => {
                #value
            }
        }
    }
}
