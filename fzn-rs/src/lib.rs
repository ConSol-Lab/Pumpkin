//! # fzn-rs
//!
//! `fzn-rs` is a crate that allows for easy parsing of FlatZinc instances in Rust.
//!
//! ## Comparison to other FlatZinc crates
//! There are two well-known crates for parsing FlatZinc files:
//! - [flatzinc](https://docs.rs/flatzinc), for parsing the original `fzn` format,
//! - and [flatzinc-serde](https://docs.rs/flatzinc-serde), for parsing `fzn.json`.
//!
//! The goal of this crate is to be able to parse both the original `fzn` format, as well as the
//! newer `fzn.json` format. Additionally, there is a derive macro that allows for strongly-typed
//! constraints as they are supported by your application. Finally, our aim is to improve the error
//! messages that are encountered when parsing invalid FlatZinc files.
//!
//! ## Typed Instance
//! The main type exposed by the crate is [`TypedInstance`], which is a fully typed representation
//! of a FlatZinc model.
//!
//! ```
//! use fzn_rs::TypedInstance;
//!
//! enum Constraints {
//!     // ...
//! }
//!
//! type Instance = TypedInstance<i32, Constraints>;
//! ```
//!
//! ## Derive Macro
//! When parsing a FlatZinc file, the result is an [`ast::Ast`]. That type describes any valid
//! FlatZinc file. However, when consuming FlatZinc, typically you need to process that AST
//! further. For example, to support the [`int_lin_le`][1] constraint, you have to validate that the
//! [`ast::Constraint`] has three arguments, and that each of the arguments has the correct type.
//!
//! When using this crate with the `derive` feature, you can instead do the following:
//! ```rust
//! use fzn_rs::ArrayExpr;
//! use fzn_rs::FlatZincConstraint;
//! use fzn_rs::VariableExpr;
//!
//! #[derive(FlatZincConstraint)]
//! pub enum MyConstraints {
//!     /// The variant name is converted to snake_case to serve as the constraint identifier by
//!     /// default.
//!     IntLinLe(ArrayExpr<i64>, ArrayExpr<VariableExpr<i64>>, i64),
//!
//!     /// If the snake_case version of the variant name is different from the constraint
//!     /// identifier, then the `#[name(...)], attribute allows you to set the constraint
//!     /// identifier explicitly.
//!     #[name("int_lin_eq")]
//!     LinearEquality(ArrayExpr<i64>, ArrayExpr<VariableExpr<i64>>, i64),
//!
//!     /// Constraint arguments can also be named, but the order determines how they are parsed
//!     /// from the AST.
//!     Element {
//!         index: VariableExpr<i64>,
//!         array: ArrayExpr<i64>,
//!         rhs: VariableExpr<i64>,
//!     },
//!
//!     /// Arguments can also be separate structs, if the enum variant has exactly one argument.
//!     #[args]
//!     IntTimes(Multiplication),
//! }
//!
//! #[derive(FlatZincConstraint)]
//! pub struct Multiplication {
//!     a: VariableExpr<i64>,
//!     b: VariableExpr<i64>,
//!     c: VariableExpr<i64>,
//! }
//! ```
//! The macro automatically implements [`FlatZincConstraint`] and will handle the parsing
//! of arguments for you.
//!
//! Similar to typed constraints, the derive macro for [`FlatZincAnnotation`] allows for easy
//! parsing of annotations:
//! ```
//! use std::rc::Rc;
//!
//! use fzn_rs::FlatZincAnnotation;
//!
//! #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
//! enum TypedAnnotation {
//!     /// Matches the snake-case atom "annotation".
//!     Annotation,
//!
//!     /// Supports nested annotations with the `#[annotation]` attribute.
//!     SomeAnnotation(#[annotation] SomeAnnotationArgs),
//! }
//!
//! #[derive(Clone, Debug, PartialEq, Eq, FlatZincAnnotation)]
//! enum SomeAnnotationArgs {
//!     /// Just as constraints, the name can be explicitly set.
//!     #[name("arg_one")]
//!     Arg1,
//!     ArgTwo(Rc<str>),
//! }
//! ```
//! Different to parsing constraints, is that annotations can be ignored. If the AST contains an
//! annotation whose name does not match one of the variants in the enum, then the annotation is
//! simply ignored.
//!
//! [1]: https://docs.minizinc.dev/en/stable/lib-flatzinc-int.html#int-lin-le

mod error;
mod typed;

pub mod ast;
#[cfg(feature = "fzn")]
pub mod fzn;

pub use error::*;
#[cfg(feature = "derive")]
pub use fzn_rs_derive::*;
pub use typed::*;
