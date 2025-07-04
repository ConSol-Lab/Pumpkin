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
//! ## Derive Macro
//! When parsing a FlatZinc file, the result is an [`ast::Ast`]. That type describes any valid
//! FlatZinc file. However, when consuming FlatZinc, typically you need to process that AST
//! further. For example, to support the [`int_lin_le`][1] constraint, you have to validate that the
//! [`ast::Constraint`] has three arguments, and that each of the arguments has the correct type.
//!
//! When using this crate with the `derive` feature, you can instead do the following:
//! ```rust
//! #[derive(FlatZincConstraint)]
//! pub enum MyConstraints {
//!     /// The variant name is converted to snake_case to serve as the constraint identifier by
//!     /// default.
//!     IntLinLe(Vec<i64>, Vec<IntVariable>, i64),
//!
//!     /// If the snake_case version of the variant name is different from the constraint
//!     /// identifier, then the `#[name(...)], attribute allows you to set the constraint
//!     /// identifier explicitly.
//!     #[name("int_lin_eq")]
//!     LinearEquality(Vec<i64>, Vec<IntVariable>, i64),
//! }
//! ```
//! The macro automatically implements [`from_ast::FlatZincConstraint`] and will handle the parsing
//! of arguments for you.
//!
//! [1]: https://docs.minizinc.dev/en/stable/lib-flatzinc-int.html#int-lin-le
#[cfg(feature = "derive")]
pub use fzn_rs_derive::*;

pub mod ast;

mod error;
mod from_ast;
#[cfg(feature = "fzn")]
mod fzn;

use std::collections::BTreeMap;
use std::rc::Rc;

use ast::SolveObjective;
use ast::Variable;
pub use error::*;
pub use from_ast::*;

#[derive(Clone, Debug)]
pub struct Instance<InstanceConstraint, ConstraintAnn = (), VariableAnn = ()> {
    /// The variables that are in the instance.
    ///
    /// The key is the identifier of the variable, and the value is the domain of the variable.
    pub variables: BTreeMap<Rc<str>, Variable<VariableAnn>>,

    /// The constraints in the instance.
    pub constraints: Vec<Constraint<InstanceConstraint, ConstraintAnn>>,

    /// The solve item indicating the type of model.
    pub solve: SolveObjective,
}

#[derive(Clone, Debug)]
pub struct Constraint<InstanceConstraint, ConstraintAnn> {
    pub constraint: ast::Node<InstanceConstraint>,
    pub annotations: Vec<ast::Node<ConstraintAnn>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntVariable {
    Identifier(Rc<str>),
    Constant(i64),
}

impl<InstanceConstraint, ConstraintAnn, VariableAnn>
    Instance<InstanceConstraint, ConstraintAnn, VariableAnn>
where
    InstanceConstraint: FlatZincConstraint,
    ConstraintAnn: FlatZincAnnotation,
    VariableAnn: FlatZincAnnotation,
{
    pub fn from_ast(ast: ast::Ast) -> Result<Self, InstanceError> {
        let variables = ast
            .variables
            .into_iter()
            .map(|(id, variable)| {
                let variable = Variable {
                    domain: variable.node.domain,
                    value: variable.node.value,
                    annotations: map_annotations(&variable.node.annotations)?,
                };

                Ok((id, variable))
            })
            .collect::<Result<_, _>>()?;

        let constraints = ast
            .constraints
            .iter()
            .map(|constraint| {
                let annotations = map_annotations(&constraint.node.annotations)?;

                let instance_constraint =
                    InstanceConstraint::from_ast(&constraint.node, &ast.arrays)?;

                Ok(Constraint {
                    constraint: ast::Node {
                        node: instance_constraint,
                        span: constraint.span,
                    },
                    annotations,
                })
            })
            .collect::<Result<_, _>>()?;

        Ok(Instance {
            variables,
            constraints,
            solve: ast.solve,
        })
    }
}

fn map_annotations<Ann: FlatZincAnnotation>(
    annotations: &[ast::Node<ast::Annotation>],
) -> Result<Vec<ast::Node<Ann>>, InstanceError> {
    annotations
        .iter()
        .filter_map(|annotation| {
            Ann::from_ast(&annotation.node)
                .map(|maybe_node| {
                    maybe_node.map(|node| ast::Node {
                        node,
                        span: annotation.span,
                    })
                })
                .transpose()
        })
        .collect()
}
