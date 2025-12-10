use quote::quote;

/// Construct a token stream that initialises a constraint with value name `value_type` and the
/// arguments described in `fields`.
pub(crate) fn initialise_value(
    identifier: &syn::Ident,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    match fields {
        // In case of named fields, the order of the fields is the order of the flatzinc arguments.
        syn::Fields::Named(fields) => {
            let arguments = fields.named.iter().enumerate().map(|(idx, field)| {
                let field_name = field
                    .ident
                    .as_ref()
                    .expect("we are in a syn::Fields::Named");
                let ty = &field.ty;

                quote! {
                    #field_name: <#ty as ::fzn_rs::FromArgument>::from_argument(
                        &constraint.node.arguments[#idx],
                    )?,
                }
            });

            quote! { #identifier { #(#arguments)* } }
        }

        syn::Fields::Unnamed(fields) => {
            let arguments = fields.unnamed.iter().enumerate().map(|(idx, field)| {
                let ty = &field.ty;

                quote! {
                    <#ty as ::fzn_rs::FromArgument>::from_argument(
                        &constraint.node.arguments[#idx],
                    )?,
                }
            });

            quote! { #identifier ( #(#arguments)* ) }
        }

        syn::Fields::Unit => quote! {
            compile_error!("A FlatZinc constraint must have at least one field")
        },
    }
}

/// Generate an implementation of `FlatZincConstraint` for enums.
pub(crate) fn flatzinc_constraint_for_enum(
    constraint_enum_name: &syn::Ident,
    data_enum: &syn::DataEnum,
) -> proc_macro2::TokenStream {
    // For every variant in the enum, create a match arm that matches the constraint name and
    // parses the constraint with the appropriate arguments.
    let constraints = data_enum.variants.iter().map(|variant| {
        // Determine the flatzinc name of the constraint.
        let name = match crate::common::get_explicit_name(variant) {
            Ok(name) => name,
            Err(_) => {
                return quote! {
                    compile_error!("Invalid usage of #[name(...)]");
                }
            }
        };

        let variant_name = &variant.ident;
        let match_expression = match crate::common::get_args_type(variant) {
            Some(constraint_type) => quote! {
                Ok(#variant_name (<#constraint_type as ::fzn_rs::FlatZincConstraint>::from_ast(constraint)?))
            },
            None => {
                let initialised_value = initialise_value(variant_name, &variant.fields);
                let expected_num_arguments = variant.fields.len();

                quote! {
                    if constraint.node.arguments.len() != #expected_num_arguments {
                        return Err(::fzn_rs::InstanceError::IncorrectNumberOfArguments {
                            expected: #expected_num_arguments,
                            actual: constraint.node.arguments.len(),
                            span: constraint.span,
                        });
                    }

                    Ok(#initialised_value)
                }
            }
        };

        quote! {
            #name => {
                #match_expression
            }
        }
    });

    quote! {
        use #constraint_enum_name::*;

        match constraint.node.name.node.as_ref() {
            #(#constraints)*
            unknown => Err(::fzn_rs::InstanceError::UnsupportedConstraint(
                String::from(unknown)
            )),
        }
    }
}
