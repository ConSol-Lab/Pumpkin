use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::DisjunctiveCheckerTask;
use pumpkin_checking::checkers::DisjunctiveEdgeFindingChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::DisjunctiveDescription;

/// The edge-finding rule of the disjunctive constraint.
#[derive(Clone, Copy, Debug)]
pub struct DisjunctiveEdgeFindingRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for DisjunctiveEdgeFindingRule<Var> {
    type Description = DisjunctiveDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("disjunctive_edge_finding")
    }

    fn create_inference_checker(
        constraint_description: &DisjunctiveDescription<Var>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }

    fn create_retention_checker(
        constraint_description: &DisjunctiveDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        Self::checker(constraint_description)
    }
}

impl<Var: IntegerVariable + 'static> DisjunctiveEdgeFindingRule<Var> {
    fn checker(
        constraint_description: &DisjunctiveDescription<Var>,
    ) -> DisjunctiveEdgeFindingChecker<Var> {
        DisjunctiveEdgeFindingChecker {
            tasks: constraint_description
                .tasks
                .iter()
                .map(|task| DisjunctiveCheckerTask {
                    start_time: task.start_time.clone(),
                    processing_time: task.processing_time,
                })
                .collect(),
        }
    }
}
