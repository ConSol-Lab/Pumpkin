use std::cmp::Reverse;

use fixedbitset::FixedBitSet;

use super::theta_lambda_tree::ThetaLambdaTree;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::containers::StorageKey;
use crate::engine::cp::propagation::contexts::propagation_context::ReadDomains;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

pub(crate) struct Disjunctive<Var: IntegerVariable + 'static> {
    tasks: Vec<Task<Var>>,
    sorted_tasks: Vec<Task<Var>>,
    elements_in_theta: FixedBitSet,
}

impl<Var: IntegerVariable + 'static> Disjunctive<Var> {
    pub(crate) fn new(mut tasks: Vec<Task<Var>>) -> Self {
        let num_tasks = tasks.len();
        tasks
            .iter_mut()
            .enumerate()
            .for_each(|(index, task)| task.id = LocalId::from(index as u32));
        Self {
            tasks: tasks.clone(),
            sorted_tasks: tasks,
            elements_in_theta: FixedBitSet::with_capacity(num_tasks),
        }
    }

    fn create_explanation<'a>(
        &'a self,
        context: &'a PropagationContextMut,
    ) -> impl Iterator<Item = Predicate> + 'a {
        self.elements_in_theta.ones().flat_map(move |index| {
            let task = &self.tasks[index];
            [
                predicate!(task.start_variable >= context.lower_bound(&task.start_variable)),
                predicate!(task.start_variable <= context.upper_bound(&task.start_variable)),
            ]
        })
    }
}

impl<Var: IntegerVariable + 'static> Propagator for Disjunctive<Var> {
    fn name(&self) -> &str {
        "Disjunctive"
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        let mut theta_lambda_tree = ThetaLambdaTree::new(&self.tasks, context.as_readonly());
        for task in self.tasks.iter() {
            self.elements_in_theta.insert(task.id.index());
            theta_lambda_tree.add_to_theta(task, context.as_readonly());
        }

        self.sorted_tasks.sort_by_key(|task| {
            Reverse(context.upper_bound(&task.start_variable) + task.processing_time)
        });

        let mut index = 0;
        let mut j = &self.sorted_tasks[index];
        let mut lct_j = context.upper_bound(&j.start_variable) + j.processing_time;

        while index < self.tasks.len() - 1 {
            if theta_lambda_tree.ect() > lct_j {
                return Err(Inconsistency::Conflict(
                    self.create_explanation(&context).collect(),
                ));
            }

            theta_lambda_tree.remove_from_theta(j);
            self.elements_in_theta.remove(j.id.index());
            theta_lambda_tree.add_to_lambda(j, context.as_readonly());

            index += 1;
            j = &self.sorted_tasks[index];
            lct_j = context.upper_bound(&j.start_variable) + j.processing_time;

            while theta_lambda_tree.ect_bar() > lct_j {
                let i = theta_lambda_tree.responsible_ect();

                if theta_lambda_tree.ect()
                    > context.lower_bound(&self.tasks[i.index()].start_variable)
                {
                    // Propagate
                    context.set_lower_bound(
                        &self.tasks[i.index()].start_variable,
                        theta_lambda_tree.ect(),
                        self.create_explanation(&context)
                            .chain({
                                let task_i = &self.tasks[i.index()];
                                [
                                    predicate!(
                                        task_i.start_variable
                                            >= context.lower_bound(&task_i.start_variable)
                                    ),
                                    predicate!(
                                        task_i.start_variable
                                            <= context.upper_bound(&task_i.start_variable)
                                    ),
                                ]
                            })
                            .collect::<PropositionalConjunction>(),
                    )?;
                }
                theta_lambda_tree.remove_from_lambda(&self.tasks[i.index()]);
            }
        }

        Ok(())
    }

    fn debug_propagate_from_scratch(&self, _context: PropagationContextMut) -> PropagationStatusCP {
        todo!()
    }

    fn initialise_at_root(
        &mut self,
        _: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::propagation::LocalId;
    use crate::engine::test_solver::TestSolver;
    use crate::propagators::disjunctive::disjunctive_propagator::Disjunctive;
    use crate::propagators::Task;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let c = solver.new_variable(4, 26);
        let d = solver.new_variable(13, 13);
        let e = solver.new_variable(5, 10);
        let f = solver.new_variable(5, 10);

        let _ = solver
            .new_propagator(Disjunctive::new(
                [
                    Task {
                        start_variable: c,
                        processing_time: 4,
                        resource_usage: 1,
                        id: LocalId::from(0),
                    },
                    Task {
                        start_variable: d,
                        processing_time: 5,
                        resource_usage: 1,
                        id: LocalId::from(1),
                    },
                    Task {
                        start_variable: e,
                        processing_time: 3,
                        resource_usage: 1,
                        id: LocalId::from(2),
                    },
                    Task {
                        start_variable: f,
                        processing_time: 3,
                        resource_usage: 1,
                        id: LocalId::from(3),
                    },
                ]
                .into_iter()
                .collect::<Vec<_>>(),
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(c), 18);
    }
}
