use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::LexLessOrEqualChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::LexLessOrEqualDescription;

/// The rule of [`super::LexLessOrEqualPropagator`], named as the rule of fzn-croc-check.
#[derive(Clone, Copy, Debug)]
pub struct LexLessOrEqualRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> LexLessOrEqualRule<Var> {
    fn checker(
        constraint_description: &LexLessOrEqualDescription<Var>,
    ) -> LexLessOrEqualChecker<Var> {
        LexLessOrEqualChecker {
            x: constraint_description.x.clone(),
            y: constraint_description.y.clone(),
        }
    }
}

impl<Var: IntegerVariable + 'static> ConflictRule for LexLessOrEqualRule<Var> {
    type Description = LexLessOrEqualDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("lex_less_or_equal")
    }

    fn create_conflict_checker(
        constraint_description: &LexLessOrEqualDescription<Var>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &LexLessOrEqualDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}
