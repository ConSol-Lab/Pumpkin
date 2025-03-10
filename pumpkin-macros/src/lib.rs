use itertools::Itertools;
use proc_macro::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::parse_macro_input;
use syn::punctuated::Punctuated;
use syn::Ident;
use syn::Token;

const EXPLANATION_TYPES: [&str; 3] = ["naive", "big-step", "pointwise"];
const OTHER_OPTIONS: [&str; 3] = [
    "--cumulative-generate-sequence",
    "--cumulative-incremental-backtracking",
    "--cumulative-allow-holes",
];

/// A macro for creating test cases for the cumulative.
///
/// It takes as input the name of a propagator (in snake case) and it creates
/// a test case for every possible combination of cumulative options
#[proc_macro]
pub fn cumulative(item: TokenStream) -> TokenStream {
    let input = syn::parse::<Ident>(item);

    let mut output = TokenStream::new();

    if let Ok(input) = input {
        let propagation_method = input.to_string();

        for explanation_type in EXPLANATION_TYPES {
            for options in OTHER_OPTIONS.into_iter().powerset() {
                let option_string = options
                    .iter()
                    .map(|argument| {
                        stringcase::snake_case(&argument[2..].split("-").skip(1).join("_"))
                    })
                    .join("_");

                let test_name = format_ident!(
                    "{}",
                    stringcase::snake_case(
                        &[
                            "cumulative",
                            &propagation_method,
                            explanation_type,
                            &option_string,
                        ]
                        .into_iter()
                        .filter(|string| !string.is_empty())
                        .join("_")
                    )
                );
                let stream: TokenStream = quote! {
                    mzn_test!(
                        #test_name,
                        "cumulative",
                        vec![
                            "--cumulative-propagation-method".to_string(),
                            stringcase::kebab_case(#propagation_method),
                            "--cumulative-explanation-type".to_string(),
                            #explanation_type.to_string(),
                            #(#options.to_string()),*
                        ]
                    );
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

/// A macro for creating test cases for the synchronisation of the cumulative.
///
/// It takes as input the name of two propagators (both in snake case) and it creates
/// a test case for every possible combination of cumulative options which checks
/// whether the statistics of the two outputs are the same.
#[proc_macro]
pub fn cumulative_synchronised(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item with Punctuated::<Ident, Token![,]>::parse_terminated)
        .into_iter()
        .collect::<Vec<_>>();

    if input.len() != 2 {
        return quote! {
            compile_error!("Expected two arguments")
        }
        .into();
    }

    let mut output = TokenStream::new();

    for explanation_type in EXPLANATION_TYPES {
        for options in OTHER_OPTIONS.into_iter().powerset() {
            let first_name = input[0].to_string();
            let second_name = input[1].to_string();

            let option_string = options
                .iter()
                .map(|argument| stringcase::snake_case(&argument[2..].split("-").skip(1).join("_")))
                .join("_");
            let test_name = format_ident!(
                "{}",
                stringcase::snake_case(
                    &[
                        "cumulative",
                        &first_name,
                        "equal_with",
                        &second_name,
                        explanation_type,
                        &option_string,
                    ]
                    .into_iter()
                    .filter(|string| !string.is_empty())
                    .join("_")
                )
            );

            let stream: TokenStream = quote! {
                #[test]
                fn #test_name() {
                    check_statistic_equality(
                        "cumulative",
                        "mzn_constraints",
                        vec![
                            "--cumulative-propagation-method".to_string(),
                            stringcase::kebab_case(stringify!(#first_name)),
                            "--cumulative-explanation-type".to_string(),
                            #explanation_type.to_string(),
                            #(#options.to_string()),*
                        ],
                        vec![
                            "--cumulative-propagation-method".to_string(),
                            stringcase::kebab_case(stringify!(#second_name)),
                            "--cumulative-explanation-type".to_string(),
                            #explanation_type.to_string(),
                            #(#options.to_string()),*
                        ],
                        &format!("equality_{}_{}_{}", #first_name, #explanation_type, #option_string),
                        &format!("equality_{}_{}_{}", #second_name, #explanation_type, #option_string),
                    );
                }
            }
            .into();

            output.extend(stream);
        }
    }

    output
}
