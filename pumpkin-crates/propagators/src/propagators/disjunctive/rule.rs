use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::InferenceChecker;
use pumpkin_checking::RetentionChecker;
use pumpkin_checking::checkers::DisjunctiveCheckerTask;
use pumpkin_checking::checkers::DisjunctiveEdgeFindingChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::MissingRetentionChecker;
use pumpkin_core::variables::IntegerVariable;

use super::disjunctive_task::ArgDisjunctiveTask;

/// The description of the disjunctive constraint: no two of the tasks overlap.
#[derive(Clone, Debug)]
pub struct DisjunctiveDescription<Var> {
    pub tasks: Vec<ArgDisjunctiveTask<Var>>,
}

impl<Var: IntegerVariable> ConstraintDescription for DisjunctiveDescription<Var> {
    fn scope(&self) -> Scope {
        Scope::from_variables(self.tasks.iter().map(|task| &task.start_time))
    }
}

/// The edge-finding rule of the disjunctive constraint.
#[derive(Clone, Copy, Debug)]
pub struct DisjunctiveEdgeFindingRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for DisjunctiveEdgeFindingRule<Var> {
    type Description = DisjunctiveDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("disjunctive_edge_finding")
    }

    fn create_inference_checker(
        description: &DisjunctiveDescription<Var>,
    ) -> impl InferenceChecker<Predicate> + 'static {
        DisjunctiveEdgeFindingChecker {
            tasks: description
                .tasks
                .iter()
                .map(|task| DisjunctiveCheckerTask {
                    start_time: task.start_time.clone(),
                    processing_time: task.processing_time,
                })
                .collect(),
        }
    }

    fn create_retention_checker(
        _: &DisjunctiveDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        MissingRetentionChecker::todo("the disjunctive edge-finding rule")
    }
}
