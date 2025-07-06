use convert_case::Case;
use convert_case::Casing;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::DeriveInput;
use syn::LitStr;

/// Get the name of the constraint or annotation from the variant. This either is converting the
/// variant name to snake case, or retrieving the value from the `#[name(...)]` attribute.
fn get_explicit_name(variant: &syn::Variant) -> syn::Result<String> {
    variant
        .attrs
        .iter()
        // Find the attribute with a `name` as the path.
        .find(|attr| attr.path().get_ident().is_some_and(|ident| ident == "name"))
        // Parse the arguments of the attribute to a string literal.
        .map(|attr| {
            attr.parse_args::<LitStr>()
                .map(|string_lit| string_lit.value())
        })
        // If no `name` attribute exists, return the snake-case version of the variant name.
        .unwrap_or_else(|| Ok(variant.ident.to_string().to_case(Case::Snake)))
}

fn variant_to_constraint_argument(variant: &syn::Variant) -> proc_macro2::TokenStream {
    // Determine the flatzinc name of the constraint.
    let name = match get_explicit_name(variant) {
        Ok(name) => name,
        Err(_) => {
            return quote! {
                compile_error!("Invalid usage of #[name(...)]");
            }
        }
    };

    let variant_ident = &variant.ident;

    let constraint_value = match &variant.fields {
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
                    )?
                }
            });

            quote! {
                #variant_ident {
                    #(#arguments),*
                }
            }
        }

        syn::Fields::Unnamed(fields) => {
            let arguments = fields.unnamed.iter().enumerate().map(|(idx, field)| {
                let ty = &field.ty;

                quote! {
                    <#ty as ::fzn_rs::FromArgument>::from_argument(
                        &constraint.arguments[#idx],
                        arrays,
                    )?
                }
            });

            quote! {
                #variant_ident(
                    #(#arguments),*
                )
            }
        }

        syn::Fields::Unit => quote! {
            compile_error!("A FlatZinc constraint must have at least one field")
        },
    };

    quote! {
        #name => {
            Ok(#constraint_value)
        },
    }
}

fn variant_to_annotation(variant: &syn::Variant) -> proc_macro2::TokenStream {
    // Determine the flatzinc annotation name.
    let name = match get_explicit_name(variant) {
        Ok(name) => name,
        Err(_) => {
            return quote! {
                compile_error!("Invalid usage of #[name(...)]");
            }
        }
    };

    let variant_ident = &variant.ident;

    match &variant.fields {
        syn::Fields::Named(fields) => {
            let num_arguments = fields.named.len();
            let arguments = fields.named.iter().enumerate().map(|(idx, field)| {
                let field_name = field
                    .ident
                    .as_ref()
                    .expect("we are in a syn::Fields::Named");
                let ty = &field.ty;

                quote! {
                    #field_name: <#ty as ::fzn_rs::FromAnnotationArgument>::from_argument(
                        &arguments[#idx],
                    )?
                }
            });

            quote! {
                ::fzn_rs::ast::Annotation::Call(::fzn_rs::ast::AnnotationCall {
                    name,
                    arguments,
                }) if name.as_ref() == #name => {
                    if arguments.len() != #num_arguments {
                        return Err(::fzn_rs::InstanceError::IncorrectNumberOfArguments {
                            expected: #num_arguments,
                            actual: arguments.len(),
                        });
                    }

                    Ok(Some(#variant_ident { #(#arguments),* }))
                }
            }
        }

        syn::Fields::Unnamed(fields) => {
            let num_arguments = fields.unnamed.len();
            let arguments = fields.unnamed.iter().enumerate().map(|(idx, field)| {
                if field.attrs.iter().any(|attr| {
                    attr.path()
                        .get_ident()
                        .is_some_and(|ident| ident == "annotation")
                }) {
                    quote! {
                        ::fzn_rs::from_nested_annotation(&arguments[#idx])?
                    }
                } else {
                    let ty = &field.ty;
                    quote! {
                        <#ty as ::fzn_rs::FromAnnotationArgument>::from_argument(
                            &arguments[#idx],
                        )?
                    }
                }
            });

            quote! {
                ::fzn_rs::ast::Annotation::Call(::fzn_rs::ast::AnnotationCall {
                    name,
                    arguments,
                }) if name.as_ref() == #name => {
                    if arguments.len() != #num_arguments {
                        return Err(::fzn_rs::InstanceError::IncorrectNumberOfArguments {
                            expected: #num_arguments,
                            actual: arguments.len(),
                        });
                    }

                    Ok(Some(#variant_ident(#(#arguments),*)))
                }
            }
        }

        syn::Fields::Unit => quote! {
            ::fzn_rs::ast::Annotation::Atom(ident) if ident.as_ref() == #name => {
                Ok(Some(#variant_ident))
            }
        },
    }
}

#[proc_macro_derive(FlatZincConstraint, attributes(name))]
pub fn derive_flatzinc_constraint(item: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(item as DeriveInput);
    let constraint_enum_name = derive_input.ident;

    let syn::Data::Enum(data_enum) = derive_input.data else {
        return quote! {
            compile_error!("derive(FlatZincConstraint) only works on enums")
        }
        .into();
    };

    let constraints = data_enum
        .variants
        .iter()
        .map(variant_to_constraint_argument);

    let token_stream = quote! {
        impl ::fzn_rs::FlatZincConstraint for #constraint_enum_name {
            fn from_ast(
                constraint: &::fzn_rs::ast::Constraint,
                arrays: &std::collections::BTreeMap<std::rc::Rc<str>, ::fzn_rs::ast::Node<::fzn_rs::ast::Array>>,
            ) -> Result<Self, ::fzn_rs::InstanceError> {
                use #constraint_enum_name::*;

                match constraint.name.node.as_ref() {
                    #(#constraints)*
                    unknown => Err(::fzn_rs::InstanceError::UnsupportedConstraint(
                        String::from(unknown)
                    )),
                }
            }
        }
    };

    token_stream.into()
}

#[proc_macro_derive(FlatZincAnnotation, attributes(name, annotation))]
pub fn derive_flatzinc_annotation(item: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(item as DeriveInput);
    let annotatation_enum_name = derive_input.ident;

    let syn::Data::Enum(data_enum) = derive_input.data else {
        return quote! {
            compile_error!("derive(FlatZincAnnotation) only works on enums")
        }
        .into();
    };

    let annotations = data_enum.variants.iter().map(variant_to_annotation);

    let token_stream = quote! {
        impl ::fzn_rs::FlatZincAnnotation for #annotatation_enum_name {
            fn from_ast(
                annotation: &::fzn_rs::ast::Annotation
            ) -> Result<Option<Self>, ::fzn_rs::InstanceError> {
                use #annotatation_enum_name::*;

                match annotation {
                    #(#annotations),*
                    _ => Ok(None),
                }
            }
        }
    };

    token_stream.into()
}
