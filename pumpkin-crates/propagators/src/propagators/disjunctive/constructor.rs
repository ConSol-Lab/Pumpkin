use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::ConstructedPropagator;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::variables::IntegerVariable;

use super::DisjunctiveDescription;
use super::DisjunctiveEdgeFindingRule;
use super::DisjunctivePropagator;
use super::disjunctive_task::ArgDisjunctiveTask;
use super::disjunctive_task::DisjunctiveTask;
use super::theta_lambda_tree::ThetaLambdaTree;

#[derive(Debug)]
pub struct DisjunctiveConstructor<Var> {
    constraint_tag: ConstraintTag,
    constraint_description: DisjunctiveDescription<Var>,
}

impl<Var> DisjunctiveConstructor<Var> {
    pub fn new(
        tasks: impl IntoIterator<Item = ArgDisjunctiveTask<Var>>,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self {
            constraint_tag,
            constraint_description: DisjunctiveDescription {
                tasks: tasks.into_iter().collect(),
            },
        }
    }
}

impl<Var: IntegerVariable + 'static> PropagatorConstructor for DisjunctiveConstructor<Var> {
    type PropagatorImpl = DisjunctivePropagator<Var>;
    type Rule = DisjunctiveEdgeFindingRule<Var>;

    fn constraint_description(&self) -> DisjunctiveDescription<Var> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        _: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> ConstructedPropagator<Self::PropagatorImpl> {
        let tasks = self
            .constraint_description
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

        let mut events_to_register = EventsToRegister::builder();
        for task in tasks.iter() {
            events_to_register =
                events_to_register.add(&task.start_time, DomainEvents::BOUNDS, task.id);
        }

        let propagator = DisjunctivePropagator {
            tasks: tasks.clone().into_boxed_slice(),
            sorted_tasks: tasks,
            theta_lambda_tree,

            inference_code,
        };

        ConstructedPropagator {
            events_to_register: events_to_register.build(),
            propagator,
        }
    }
}
