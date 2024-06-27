//! Structures which represent certain [predicates](https://en.wikipedia.org/wiki/Predicate_(mathematical_logic)).
//!
//! The solver only utilizes the following types of predicates:
//! - **Predicates over integers** - These [`IntegerPredicate`]s specify atomic constraints of the
//!   form `[x >= v]`, `[x <= v]`, `[x == v]`, and `[x != v]`.
//! - **Predicates over literals** - These [`Predicate::Literal`]s specify [`Literal`]s which are
//!   linked to the aforementioned [`IntegerPredicate`]s through the [`VariableLiteralMappings`].
//! - **Always True/False** - The [`Predicate::True`]/[`Predicate::False`] specify logical
//!   predicates which are always true/false.
//!
//! In general, these [`Predicate`]s are used to represent propagations, explanations or decisions.
pub(crate) mod integer_predicate;
pub(crate) mod predicate;
pub(crate) mod predicate_constructor;
#[cfg(doc)]
use crate::engine::predicates::integer_predicate::IntegerPredicate;
#[cfg(doc)]
use crate::engine::predicates::predicate::Predicate;
#[cfg(doc)]
use crate::engine::variables::IntegerVariable;
#[cfg(doc)]
use crate::engine::variables::Literal;
#[cfg(doc)]
use crate::engine::VariableLiteralMappings;
