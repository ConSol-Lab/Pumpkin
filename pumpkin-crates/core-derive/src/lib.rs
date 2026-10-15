//! Derive macros for `pumpkin-core`, re-exported by it.

use proc_macro::TokenStream;
use quote::quote;
use syn::Data;
use syn::DeriveInput;
use syn::Index;
use syn::parse_macro_input;

/// Implements `ScopeItem` for a struct by adding every field to the scope, so that no field that
/// holds variables can be left out of it. Every field has to implement `ScopeItem`.
#[proc_macro_derive(ScopeItem)]
pub fn derive_scope_item(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let name = &input.ident;

    let Data::Struct(data_struct) = &input.data else {
        return syn::Error::new_spanned(name, "ScopeItem can only be derived for structs")
            .to_compile_error()
            .into();
    };

    let field_types = data_struct.fields.iter().map(|field| &field.ty);
    let field_accesses =
        data_struct
            .fields
            .iter()
            .enumerate()
            .map(|(position, field)| match &field.ident {
                Some(identifier) => quote!(#identifier),
                None => {
                    let index = Index::from(position);
                    quote!(#index)
                }
            });

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    let existing_predicates = where_clause
        .map(|where_clause| where_clause.predicates.iter().collect::<Vec<_>>())
        .unwrap_or_default();

    quote! {
        #[automatically_derived]
        impl #impl_generics ::pumpkin_core::checkers::ScopeItem for #name #type_generics
        where
            #(#existing_predicates,)*
            #(#field_types: ::pumpkin_core::checkers::ScopeItem,)*
        {
            fn add_to_scope(&self, scope: &mut ::pumpkin_core::checkers::Scope) {
                let _ = &scope;
                #(::pumpkin_core::checkers::ScopeItem::add_to_scope(&self.#field_accesses, scope);)*
            }
        }
    }
    .into()
}
