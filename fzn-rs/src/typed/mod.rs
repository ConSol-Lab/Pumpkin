mod arrays;
mod constraint;
mod flatzinc_annotation;
mod flatzinc_constraint;
mod from_literal;
mod instance;

use std::rc::Rc;

pub use arrays::*;
pub use constraint::*;
pub use flatzinc_annotation::*;
pub use flatzinc_constraint::*;
pub use from_literal::*;
pub use instance::*;

/// Models a variable in the FlatZinc AST. Since `var T` is a subtype of `T`, a variable can also
/// be a constant.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableExpr<T> {
    Identifier(Rc<str>),
    Constant(T),
}
