use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::ConflictChecker;
use pumpkin_checking::checkers::DisjunctiveCheckerTask;
use pumpkin_checking::checkers::DisjunctiveEdgeFindingChecker;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::variables::IntegerVariable;

use super::DisjunctiveDescription;

/// The inference rule of the propagator.
#[derive(Clone, Copy, Debug)]
pub struct DisjunctiveEdgeFindingRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for DisjunctiveEdgeFindingRule<Var> {
    type Description = DisjunctiveDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("disjunctive_edge_finding")
    }

    fn create_conflict_checker(
        constraint_description: &DisjunctiveDescription<Var>,
    ) -> impl ConflictChecker<Predicate> + 'static {
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
