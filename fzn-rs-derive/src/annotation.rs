use quote::quote;

pub(crate) fn initialise_value(
    value_type: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let field_values = fields.iter().enumerate().map(|(idx, field)| {
        let ty = &field.ty;

        // If the field has a name, then prepend the field value with the name: `name: value`.
        let value_prefix = if let Some(ident) = &field.ident {
            quote! { #ident: }
        } else {
            quote! {}
        };

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

    let value_initialiser = match fields {
        syn::Fields::Named(_) => quote! { #value_type { #(#field_values),* } },
        syn::Fields::Unnamed(_) => quote! { #value_type ( #(#field_values),* ) },
        syn::Fields::Unit => quote! { #value_type },
    };

    let num_arguments = fields.len();

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
