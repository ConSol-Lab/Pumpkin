mod consistency_checker;
mod domain_consistency_checker;
mod scope;
mod variable;
mod witness;
mod witness_generator;

pub use consistency_checker::*;
pub use domain_consistency_checker::*;
pub use scope::*;
pub use variable::*;
pub use witness::*;
pub use witness_generator::*;

use crate::propagation::Domains;

#[deprecated = "only here to aid refactoring"]
#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
pub struct DefaultChecker;

#[allow(deprecated, reason = "only here to aid refactoring")]
impl ConsistencyChecker for DefaultChecker {
    fn check_consistency(&self, _: Domains<'_>, _: &Scope) -> bool {
        true
    }
}
