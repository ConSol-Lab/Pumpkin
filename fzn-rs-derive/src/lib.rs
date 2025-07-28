mod annotation;
mod common;
mod constraint;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::DeriveInput;

#[proc_macro_derive(FlatZincConstraint, attributes(name, args))]
pub fn derive_flatzinc_constraint(item: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(item as DeriveInput);

    let type_name = derive_input.ident;
    let implementation = match &derive_input.data {
        syn::Data::Struct(data_struct) => {
            let expected_num_arguments = data_struct.fields.len();

            let struct_initialiser = constraint::initialise_value(&type_name, &data_struct.fields);
            quote! {
                if constraint.node.arguments.len() != #expected_num_arguments {
                    return Err(::fzn_rs::InstanceError::IncorrectNumberOfArguments {
                        expected: #expected_num_arguments,
                        actual: constraint.node.arguments.len(),
                        span: constraint.span,
                    });
                }

                Ok(#struct_initialiser)
            }
        }
        syn::Data::Enum(data_enum) => {
            constraint::flatzinc_constraint_for_enum(&type_name, data_enum)
        }
        syn::Data::Union(_) => quote! {
            compile_error!("Cannot implement FlatZincConstraint on unions.")
        },
    };

    let token_stream = quote! {
        #[automatically_derived]
        impl ::fzn_rs::FlatZincConstraint for #type_name {
            fn from_ast(
                constraint: &::fzn_rs::ast::Node<::fzn_rs::ast::Constraint>,
            ) -> Result<Self, ::fzn_rs::InstanceError> {
                #implementation
            }
        }
    };

    token_stream.into()
}

#[proc_macro_derive(FlatZincAnnotation, attributes(name, annotation, args))]
pub fn derive_flatzinc_annotation(item: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(item as DeriveInput);
    let annotatation_enum_name = derive_input.ident;

    let implementation = match derive_input.data {
        syn::Data::Struct(data_struct) => {
            let initialised_values =
                annotation::initialise_value(&annotatation_enum_name, &data_struct.fields);

            let expected_num_arguments = data_struct.fields.len();

            quote! {
                match &annotation.node {
                    ::fzn_rs::ast::Annotation::Call(::fzn_rs::ast::AnnotationCall {
                        name,
                        arguments,
                    }) => {
                        #initialised_values
                    }

                    _ => return Err(::fzn_rs::InstanceError::IncorrectNumberOfArguments {
                        expected: #expected_num_arguments,
                        actual: 0,
                        span: annotation.span,
                    }),
                }
            }
        }
        syn::Data::Enum(data_enum) => {
            let annotations = data_enum
                .variants
                .iter()
                .map(annotation::variant_to_annotation);

            quote! {
                use #annotatation_enum_name::*;

                match &annotation.node {
                    #(#annotations),*
                    _ => Ok(None),
                }
            }
        }
        syn::Data::Union(_) => quote! {
            compile_error!("Cannot implement FlatZincAnnotation on unions.")
        },
    };

    let token_stream = quote! {
        #[automatically_derived]
        impl ::fzn_rs::FlatZincAnnotation for #annotatation_enum_name {
            fn from_ast(
                annotation: &::fzn_rs::ast::Node<::fzn_rs::ast::Annotation>,
            ) -> Result<Option<Self>, ::fzn_rs::InstanceError> {
                #implementation
            }
        }
    };

    token_stream.into()
}
