#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    // missing_docs,
    trivial_casts,
    trivial_numeric_casts,
    unused_extern_crates,
    unused_import_braces,
    unused_qualifications,
    unused_results
)]
pub mod basic_types;
pub mod constraints;
pub mod encoders;
pub mod engine;
pub mod optimisation;
pub mod propagators;
pub mod pumpkin_asserts;
