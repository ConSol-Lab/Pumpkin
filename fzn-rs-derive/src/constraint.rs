use quote::quote;

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
                        &constraint.arguments[#idx],
                        arrays,
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
                        &constraint.arguments[#idx],
                        arrays,
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
        let value = match crate::common::get_args_type(variant) {
            Some(constraint_type) => quote! {
                #variant_name (<#constraint_type as ::fzn_rs::FlatZincConstraint>::from_ast(constraint, arrays)?)
            },
            None => initialise_value(variant_name, &variant.fields),
        };

        quote! {
            #name => {
                Ok(#value)
            },
        }
    });

    quote! {
        use #constraint_enum_name::*;

        match constraint.name.node.as_ref() {
            #(#constraints)*
            unknown => Err(::fzn_rs::InstanceError::UnsupportedConstraint(
                String::from(unknown)
            )),
        }
    }
}
