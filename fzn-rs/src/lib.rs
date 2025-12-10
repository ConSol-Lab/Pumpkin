//! # fzn-rs
//!
//! `fzn-rs` is a crate that allows for easy parsing of FlatZinc instances in Rust. It facilitates
//! type-driven parsing of a FlatZinc file using derive macros.
//!
//! ## Features
//! - `fzn-parser`: Include the parser for fzn files in the traditional `.fzn` format. In the
//!   future, a parser for the JSON format will be included as well, behind a separate feature.
//! - `derive`: Include the derive macro's to parse the AST into a strongly-typed model.
//!
//! ## Example
//! ```
//! use fzn_rs::ArrayExpr;
//! use fzn_rs::FlatZincConstraint;
//! use fzn_rs::TypedInstance;
//! use fzn_rs::VariableExpr;
//!
//! /// The FlatZincConstraint derive macro enables the parsing of a strongly typed constraint
//! /// based on the FlatZinc Ast.
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
//!
//! /// The `TypedInstance` is parameterized by the constraint type, as well as any annotations you
//! /// may need to parse. It uses `i64` to represent integers.
//! type MyInstance = TypedInstance<i64, MyConstraints>;
//!
//! fn parse_flatzinc(source: &str) -> MyInstance {
//!     // First, the source string is parsed into a structured representation.
//!     //
//!     // Note: the `fzn_rs::fzn` module is only available with the `fzn-parser` feature enabled.
//!     let ast = fzn_rs::fzn::parse(source).expect("source is valid flatzinc");
//!
//!     // Then, the strongly-typed instance is created from the AST
//!     MyInstance::from_ast(ast).expect("type-checking passes")
//! }
//! ```
//!
//! ## Derive Macros
//! When parsing a FlatZinc file, the result is an [`ast::Ast`]. That type describes any valid
//! FlatZinc file. However, when consuming FlatZinc, typically you need to process that AST
//! further. For example, to support the [`int_lin_le`][1] constraint, you have to validate that the
//! [`ast::Constraint`] has three arguments, and that each of the arguments has the correct type.
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
//! ## Comparison to other FlatZinc crates
//! There are two well-known crates for parsing FlatZinc files:
//! - [flatzinc](https://docs.rs/flatzinc), for parsing the original `fzn` format,
//! - and [flatzinc-serde](https://docs.rs/flatzinc-serde), for parsing `fzn.json`.
//!
//! These crates produce what we call the [`ast::Ast`] in this crate, although the concrete types
//! can be different. `fzn-rs` builds the strong typing of constraints and annotations on-top of
//! a unified AST for both file formats. Finally, our aim is to improve the error messages that
//! are encountered when parsing invalid FlatZinc files.
//!
//! [1]: https://docs.minizinc.dev/en/stable/lib-flatzinc-int.html#int-lin-le

mod error;
mod typed;

pub mod ast;
pub mod fzn;

pub use error::*;
pub use fzn_rs_derive::*;
pub use typed::*;
