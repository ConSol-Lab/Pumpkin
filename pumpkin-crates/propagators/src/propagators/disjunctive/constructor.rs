use pumpkin_checking::checkers::DisjunctiveCheckerTask;
use pumpkin_checking::checkers::DisjunctiveEdgeFindingChecker;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::variables::IntegerVariable;

use super::DisjunctivePropagator;
use super::disjunctive_task::ArgDisjunctiveTask;
use super::disjunctive_task::DisjunctiveTask;
use super::theta_lambda_tree::ThetaLambdaTree;
use crate::propagators::disjunctive::DisjunctiveEdgeFinding;

#[derive(Debug)]
pub struct DisjunctiveConstructor<Var> {
    constraint_tag: ConstraintTag,
    tasks: Vec<ArgDisjunctiveTask<Var>>,
}

impl<Var> DisjunctiveConstructor<Var> {
    pub fn new(
        tasks: impl IntoIterator<Item = ArgDisjunctiveTask<Var>>,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self {
            constraint_tag,
            tasks: tasks.into_iter().collect(),
        }
    }
}

impl<Var: IntegerVariable + 'static> PropagatorConstructor for DisjunctiveConstructor<Var> {
    type PropagatorImpl = DisjunctivePropagator<Var>;

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let tasks = self
            .tasks
            .into_iter()
            .enumerate()
            .map(|(index, task)| DisjunctiveTask {
                start_time: task.start_time.clone(),
                processing_time: task.processing_time,
                id: LocalId::from(index as u32),
            })
            .collect::<Vec<_>>();
        let theta_lambda_tree = ThetaLambdaTree::new(&tasks);

        let mut registration = EventsToRegister::builder();
        for task in tasks.iter() {
            registration = registration.add(&task.start_time, DomainEvents::BOUNDS, task.id);
        }

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
            self.constraint_tag,
            DisjunctiveEdgeFinding,
            DisjunctiveEdgeFindingChecker {
                tasks: tasks
                    .iter()
                    .map(|task| DisjunctiveCheckerTask {
                        start_time: task.start_time.clone(),
                        processing_time: task.processing_time,
                    })
                    .collect(),
            },
        );

        let propagator = DisjunctivePropagator {
            tasks: tasks.clone().into_boxed_slice(),
            sorted_tasks: tasks,
            theta_lambda_tree,

            inference_code,
        };

        PropagatorSpec {
            registration: registration.build(),
            checkers: checkers.build(),
            propagator,
        }
    }
}
