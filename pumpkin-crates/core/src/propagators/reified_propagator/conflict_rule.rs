use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::BoxedChecker;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::ReifiedChecker;

use super::HalfReifiedDescription;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;

/// The rule `r -> C` of the half reification of a constraint `C` with the rule `Rule`.
///
/// Its name is `half_reified(<name of Rule>)`: its inferences are checked by a different checker
/// than those of `Rule`, since their nogoods contain the reification literal.
#[derive(Clone, Copy, Debug)]
pub struct HalfReified<Rule>(PhantomData<Rule>);

impl<Rule: ConflictRule> ConflictRule for HalfReified<Rule> {
    type Description = HalfReifiedDescription<Rule::Description>;

    fn name() -> Cow<'static, str> {
        Cow::Owned(format!("half_reified({})", Rule::name()))
    }

    fn create_conflict_checker(
        constraint_description: &Self::Description,
    ) -> impl ConflictChecker<Predicate> + 'static {
        ReifiedChecker {
            inner: BoxedChecker::new(Box::new(Rule::create_conflict_checker(
                &constraint_description.inner,
            ))),
            reification_literal: constraint_description.reification_literal,
        }
    }
}
