//! A variable, in the context of the solver, is a view onto a domain. It may forward domain
//! information unaltered, or apply transformations which can be performed without the need of
//! constraints.

mod affine_view;
mod any_integer;
mod constant;
mod domain_generator_iterator;
mod domain_id;
mod integer_variable;
mod transformable_variable;

pub use affine_view::AffineView;
pub use any_integer::AnyInteger;
pub(crate) use domain_generator_iterator::DomainGeneratorIterator;
pub use domain_id::DomainId;
pub use integer_variable::IntegerVariable;
pub use transformable_variable::TransformableVariable;
