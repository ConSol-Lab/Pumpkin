use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::BoxedChecker;
use pumpkin_checking::BoxedRetentionChecker;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::ReifiedChecker;
use pumpkin_checking::checkers::ReifiedRetentionChecker;

use crate::checkers::Scope;
use crate::checkers::ScopeItem;
use crate::predicates::Predicate;
use crate::propagation::ConflictRule;
use crate::propagation::ConstraintDescription;
use crate::propagation::LocalId;
use crate::variables::Literal;

/// The description of a constraint that only has to hold when the reification literal is true.
#[derive(Clone, Debug)]
pub struct HalfReifiedDescription<Description> {
    /// The description of the constraint that is reified.
    pub inner: Description,
    pub reification_literal: Literal,
}

impl<Description: ConstraintDescription> ConstraintDescription
    for HalfReifiedDescription<Description>
{
    fn scope(&self) -> Scope {
        // Whether the inner constraint has to hold depends on the reification literal, so the
        // literal is part of the scope, under a local id after those of the inner constraint.
        let mut scope = self.inner.scope();
        let literal_id = scope
            .domains()
            .map(|(local_id, _)| local_id.successor())
            .max()
            .unwrap_or(LocalId::from(0));
        self.reification_literal
            .add_to_scope(&mut scope, literal_id);
        scope
    }
}

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
