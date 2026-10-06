use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::BoxedConflictChecker;
use pumpkin_checking::BoxedRetentionChecker;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::ReifiedChecker;
use pumpkin_checking::checkers::ReifiedRetentionChecker;

use super::HalfReifiedDescription;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;

/// The rule `r -> C` of the half reification of a constraint `C` with the rule `Rule`.
///
/// Its name is `HalfReified(<name of Rule>)`.
#[derive(Clone, Copy, Debug)]
pub struct HalfReified<Rule>(PhantomData<Rule>);

impl<Rule: ConflictRule> ConflictRule for HalfReified<Rule> {
    type Description = HalfReifiedDescription<Rule::Description>;

    fn name() -> Cow<'static, str> {
        Cow::Owned(format!("HalfReified({})", Rule::name()))
    }

    fn create_conflict_checker(
        constraint_description: &Self::Description,
    ) -> impl ConflictChecker<Predicate> + 'static {
        ReifiedChecker {
            inner: BoxedConflictChecker::new(Box::new(Rule::create_conflict_checker(
                &constraint_description.inner,
            ))),
            reification_literal: constraint_description.reification_literal,
        }
    }

    fn create_retention_checker(
        constraint_description: &Self::Description,
    ) -> impl RetentionChecker<Predicate> + 'static {
        ReifiedRetentionChecker {
            inner: BoxedRetentionChecker::new(Rule::create_retention_checker(
                &constraint_description.inner,
            )),
            reification_literal: constraint_description.reification_literal,
        }
    }
}
