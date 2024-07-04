//! Contains structures for tie-breaking; these structures provide an interface for deciding
//! between two variables when there is a tie between them (for example during variable
//! selection there can be two variables with the same smallest value in the domain).
//!
//! The responsibility of a [`TieBreaker`] is two-fold:
//! - First of all, it should have the ability to break arbitrary ties between values, for example,
//!   if we consider the [`Smallest`] strategy then a tie could occur when two variables have the
//!   same lower-bound; one tie-breaking strategy could be to simply pick the first element that was
//!   encountered (implemented in [`InOrderTieBreaker`]) but others could be considered.
//! - Secondly, it should keep track of which variable to consider based on the value (and based on
//!   the [`Direction`]); if we once again look at the example of [`Smallest`] then the
//!   [`TieBreaker`] should only consider tie-breaking between variables with the same value or
//!   update the "best" variable found so far. For example, considering the [`Smallest`]
//!   [`VariableSelector`], if we have two variables, `x` with lower-bound 5 and `y` with
//!   lower-bound 6 then the [`TieBreaker`] should only consider `x` as potential candidate since it
//!   has a strictly lower value and the direction of this [`VariableSelector`] is
//!   [`Direction::Minimum`].
//!
//! The following example shows how a simple [`TieBreaker`] ([`InOrderTieBreaker`]) will
//! select the first variable with the lowest-value that it has found.
//!
//! ```rust
//! # use pumpkin_lib::branching::InOrderTieBreaker;
//! # use pumpkin_lib::engine::variables::DomainId;
//! # use pumpkin_lib::branching::Direction;
//! # use pumpkin_lib::branching::TieBreaker;
//! let mut breaker = InOrderTieBreaker::new(Direction::Minimum);
//!
//! // We consider 3 variables, where only variables with ID 1 and ID 2 should be considered.
//! // We expect the variable with ID 1 to be selected since it was the first one with
//! // the minimum value which was considered.
//! breaker.consider(DomainId::new(0), 10);
//! breaker.consider(DomainId::new(1), 5);
//! breaker.consider(DomainId::new(2), 5);
//!
//! let selected = breaker.select();
//! assert!(selected.is_some());
//! assert_eq!(selected.unwrap(), DomainId::new(1));
//! ```
//!
//! # Note
//! Currently, the tie-breaking is only used within variable selection strategies (hence the fact
//! that it is located in [`crate::branching`]) to determine which variable to select when there are
//! multiple variables with the same value (for an example, see [`Smallest`]). However, the
//! structure of the trait ensures that it is generally usable.

mod in_order_tie_breaker;
mod random_tie_breaker;
mod tie_breaker;

pub use in_order_tie_breaker::*;
pub use tie_breaker::*;

#[cfg(doc)]
use crate::branching::Smallest;
#[cfg(doc)]
use crate::branching::VariableSelector;
