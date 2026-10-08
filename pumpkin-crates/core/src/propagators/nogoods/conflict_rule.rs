use std::borrow::Cow;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::NogoodChecker;

use super::NogoodDescription;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;

/// The rule of a nogood: when all but one of its predicates hold, the last one is false.
#[derive(Clone, Copy, Debug)]
pub struct UnitNogoodRule;

impl ConflictRule for UnitNogoodRule {
    type Description = NogoodDescription;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("nogood")
    }

    fn create_conflict_checker(
        constraint_description: &NogoodDescription,
    ) -> impl ConflictChecker<Predicate> + 'static {
        NogoodChecker {
            nogood: constraint_description.nogood.clone(),
        }
    }
}
