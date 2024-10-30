use itertools::Itertools;
use proc_macro::TokenStream;
use quote::quote;
use syn::Ident;

const EXPLANATION_TYPES: [&str; 3] = ["naive", "big-step", "pointwise"];
const OTHER_OPTIONS: [&str; 2] = [
    "--cumulative-generate-sequence",
    "--cumulative-incremental-backtracking",
];

#[proc_macro]
pub fn cumulative(item: TokenStream) -> TokenStream {
    let input = syn::parse::<Ident>(item);

    let mut output = TokenStream::new();

    if let Ok(input) = input {
        let propagation_method = input.to_string();

        for explanation_type in EXPLANATION_TYPES {
            for inputs in (0..OTHER_OPTIONS.len())
                .tuple_combinations()
                .map(|(start, end)| &OTHER_OPTIONS[start..end])
            {
                let options = inputs
                    .iter()
                    .map(|argument| {
                        stringcase::snake_case(&argument[2..].split("-").skip(1).join("_"))
                    })
                    .join("_");
                let flag_arguments = inputs.join(" ");

                let stream: TokenStream = quote! {
                paste::item! {
                        mzn_test!(
                            [< cumulative_ #propagation_method _ #explanation_type _ #options>],
                            "cumulative",
                            vec!["--cumulative-propagation-method", &stringcase::kebab_case(#propagation_method),"--cumulative-explanation-type", #explanation_type, #flag_arguments]
                        );
                }
            }
            .into();

                output.extend(stream);
            }
        }

        output
    } else {
        quote! {
            compile_error!("Could not parse the input as an identifier")
        }
        .into()
    }
}
