use convert_case::Case;
use convert_case::Casing;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::DeriveInput;
use syn::LitStr;

fn get_constraint_name(variant: &syn::Variant) -> syn::Result<String> {
    variant
        .attrs
        .iter()
        .find_map(|attr| {
            let ident = attr.path().get_ident()?;

            if ident != "name" {
                return None;
            }

            match attr.parse_args::<LitStr>() {
                Ok(string_lit) => Some(Ok(string_lit.value())),
                Err(e) => Some(Err(e)),
            }
        })
        .unwrap_or_else(|| Ok(variant.ident.to_string().to_case(Case::Snake)))
}

fn variant_to_constraint_argument(variant: &syn::Variant) -> proc_macro2::TokenStream {
    let name = match get_constraint_name(variant) {
        Ok(name) => name,
        Err(_) => {
            return quote! {
                compiler_error!("Invalid usage of #[name(...)");
            }
        }
    };
    let variant_ident = &variant.ident;

    let constraint_value = match &variant.fields {
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
            compiler_error!("A FlatZinc constraint must have at least one field")
        },
    };

    quote! {
        #name => {
            Ok(#constraint_value)
        },
    }
}

#[proc_macro_derive(FlatZincConstraint, attributes(name))]
pub fn derive_flatzinc_constraint(item: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(item as DeriveInput);
    let constraint_enum_name = derive_input.ident;

    let syn::Data::Enum(data_enum) = derive_input.data else {
        return quote! {
            compiler_error!("derive(FlatZincConstraint) only works on enums")
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
