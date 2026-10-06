use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::BoxedChecker;
use pumpkin_checking::BoxedRetentionChecker;
use pumpkin_checking::InferenceChecker;
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

    fn create_inference_checker(
        description: &Self::Description,
    ) -> impl InferenceChecker<Predicate> + 'static {
        ReifiedChecker {
            inner: BoxedChecker::new(Box::new(Rule::create_inference_checker(&description.inner))),
            reification_literal: description.reification_literal,
        }
    }

    fn create_retention_checker(
        description: &Self::Description,
    ) -> impl RetentionChecker<Predicate> + 'static {
        ReifiedRetentionChecker {
            inner: BoxedRetentionChecker::new(Rule::create_retention_checker(&description.inner)),
            reification_literal: description.reification_literal,
        }
    }
}
