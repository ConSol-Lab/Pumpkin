use convert_case::Case;
use convert_case::Casing;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::DataEnum;
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

fn initialise_value(identifier: &syn::Ident, fields: &syn::Fields) -> proc_macro2::TokenStream {
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
                let ty = &field.ty;

                if field.attrs.iter().any(|attr| {
                    attr.path()
                        .get_ident()
                        .is_some_and(|ident| ident == "annotation")
                }) {
                    quote! {
                        <#ty as ::fzn_rs::FromNestedAnnotation>::from_argument(
                            &arguments[#idx],
                        )?
                    }
                } else {
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

/// Returns the type of the arguments for the constraint if the variant has exactly the following
/// shape:
///
/// ```ignore
/// #[args]
/// Variant(Type)
/// ```
fn get_constraint_args_type(variant: &syn::Variant) -> Option<&syn::Type> {
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

/// Generate an implementation of `FlatZincConstraint` for enums.
fn flatzinc_constraint_for_enum(
    constraint_enum_name: &syn::Ident,
    data_enum: &DataEnum,
) -> proc_macro2::TokenStream {
    let constraints = data_enum.variants.iter().map(|variant| {
        // Determine the flatzinc name of the constraint.
        let name = match get_explicit_name(variant) {
            Ok(name) => name,
            Err(_) => {
                return quote! {
                    compile_error!("Invalid usage of #[name(...)]");
                }
            }
        };

        let variant_name = &variant.ident;
        let value = match get_constraint_args_type(variant) {
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

#[proc_macro_derive(FlatZincConstraint, attributes(name, args))]
pub fn derive_flatzinc_constraint(item: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(item as DeriveInput);

    let type_name = derive_input.ident;
    let implementation = match &derive_input.data {
        syn::Data::Struct(data_struct) => {
            let struct_initialiser = initialise_value(&type_name, &data_struct.fields);
            quote! { Ok(#struct_initialiser) }
        }
        syn::Data::Enum(data_enum) => flatzinc_constraint_for_enum(&type_name, data_enum),
        syn::Data::Union(_) => quote! {
            compile_error!("Cannot implement FlatZincConstraint on unions.")
        },
    };

    let token_stream = quote! {
        #[automatically_derived]
        impl ::fzn_rs::FlatZincConstraint for #type_name {
            fn from_ast(
                constraint: &::fzn_rs::ast::Constraint,
                arrays: &std::collections::BTreeMap<std::rc::Rc<str>, ::fzn_rs::ast::Node<::fzn_rs::ast::Array>>,
            ) -> Result<Self, ::fzn_rs::InstanceError> {
                #implementation
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
        #[automatically_derived]
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
