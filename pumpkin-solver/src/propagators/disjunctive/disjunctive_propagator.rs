use std::cmp::Reverse;

use fixedbitset::FixedBitSet;

use super::disjunctive_task::ArgDisjunctiveTask;
use super::disjunctive_task::DisjunctiveTask;
use super::theta_lambda_tree::ThetaLambdaTree;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::containers::StorageKey;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorInitialisationContext;
use crate::engine::DomainEvents;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

pub(crate) struct Disjunctive<Var: IntegerVariable + 'static> {
    tasks: Box<[DisjunctiveTask<Var>]>,
    sorted_tasks: Vec<DisjunctiveTask<Var>>,
    elements_in_theta: FixedBitSet,
}

impl<Var: IntegerVariable + 'static> Disjunctive<Var> {
    pub(crate) fn new(tasks: impl IntoIterator<Item = ArgDisjunctiveTask<Var>>) -> Self {
        let tasks = tasks
            .into_iter()
            .enumerate()
            .map(|(index, task)| DisjunctiveTask {
                start_variable: task.start_variable.clone(),
                processing_time: task.processing_time,
                id: LocalId::from(index as u32),
            })
            .collect::<Vec<_>>();

        let num_tasks = tasks.len();
        Self {
            tasks: tasks.clone().into_boxed_slice(),
            sorted_tasks: tasks,
            elements_in_theta: FixedBitSet::with_capacity(num_tasks),
        }
    }
}

fn create_theta_explanation<'a, Var: IntegerVariable>(
    tasks: &'a [DisjunctiveTask<Var>],
    elements_in_theta: &'a FixedBitSet,
    context: &'a PropagationContextMut,
) -> impl Iterator<Item = Predicate> + 'a {
    elements_in_theta.ones().flat_map(move |index| {
        let task = &tasks[index];
        [
            predicate!(task.start_variable >= context.lower_bound(&task.start_variable)),
            predicate!(task.start_variable <= context.upper_bound(&task.start_variable)),
        ]
    })
}

fn edge_finding<Var: IntegerVariable, SortedTaskVar: IntegerVariable>(
    context: &mut PropagationContextMut,
    tasks: &[DisjunctiveTask<Var>],
    sorted_tasks: &mut [DisjunctiveTask<SortedTaskVar>],
    elements_in_theta: &mut FixedBitSet,
) -> PropagationStatusCP {
    let mut theta_lambda_tree = ThetaLambdaTree::new(tasks, context.as_readonly());
    for task in tasks.iter() {
        elements_in_theta.insert(task.id.index());
        theta_lambda_tree.add_to_theta(task, context.as_readonly());
    }

    sorted_tasks.sort_by_key(|task| {
        Reverse(context.upper_bound(&task.start_variable) + task.processing_time)
    });

    let mut index = 0;
    let mut j = &sorted_tasks[index];
    let mut lct_j = context.upper_bound(&j.start_variable) + j.processing_time;

    while index < tasks.len() - 1 {
        if theta_lambda_tree.ect() > lct_j {
            return Err(Inconsistency::Conflict(
                create_theta_explanation(tasks, elements_in_theta, context).collect(),
            ));
        }

        theta_lambda_tree.remove_from_theta(j);
        elements_in_theta.remove(j.id.index());
        theta_lambda_tree.add_to_lambda(j, context.as_readonly());

        index += 1;
        j = &sorted_tasks[index];
        lct_j = context.upper_bound(&j.start_variable) + j.processing_time;

        while theta_lambda_tree.ect_bar() > lct_j {
            let i = theta_lambda_tree.responsible_ect();

            if theta_lambda_tree.ect() > context.lower_bound(&tasks[i.index()].start_variable) {
                // Propagate
                context.set_lower_bound(
                    &tasks[i.index()].start_variable,
                    theta_lambda_tree.ect(),
                    create_theta_explanation(tasks, elements_in_theta, context)
                        .chain({
                            let task_i = &tasks[i.index()];
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
            theta_lambda_tree.remove_from_lambda(&tasks[i.index()]);
        }
    }

    Ok(())
}

impl<Var: IntegerVariable + 'static> Propagator for Disjunctive<Var> {
    fn name(&self) -> &str {
        "Disjunctive"
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        edge_finding(
            &mut context,
            &self.tasks,
            &mut self.sorted_tasks,
            &mut self.elements_in_theta,
        )?;

        let reverse_tasks = self
            .tasks
            .iter()
            .map(|task| DisjunctiveTask {
                start_variable: task.start_variable.offset(task.processing_time).scaled(-1),
                processing_time: task.processing_time,
                id: task.id,
            })
            .collect::<Vec<_>>();
        edge_finding(
            &mut context,
            &reverse_tasks,
            &mut reverse_tasks.clone(),
            &mut self.elements_in_theta,
        )
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let mut sorted_tasks = self.sorted_tasks.clone();
        let mut elements_in_theta = self.elements_in_theta.clone();
        edge_finding(
            &mut context,
            &self.tasks,
            &mut sorted_tasks,
            &mut elements_in_theta,
        )?;
        let reverse_tasks = self
            .tasks
            .iter()
            .map(|task| DisjunctiveTask {
                start_variable: task.start_variable.clone(),
                processing_time: task.processing_time,
                id: task.id,
            })
            .collect::<Vec<_>>();
        edge_finding(
            &mut context,
            &reverse_tasks,
            &mut reverse_tasks.clone(),
            &mut elements_in_theta,
        )
    }

    fn initialise_at_root(
        &mut self,
        context: &mut PropagatorInitialisationContext,
    ) -> Result<(), PropositionalConjunction> {
        self.tasks.iter().for_each(|task| {
            let _ = context.register(task.start_variable.clone(), DomainEvents::BOUNDS, task.id);
        });
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::test_solver::TestSolver;
    use crate::propagators::disjunctive::disjunctive_propagator::Disjunctive;
    use crate::propagators::disjunctive_task::ArgDisjunctiveTask;

    #[test]
    fn propagator_propagates_lower_bound() {
        let mut solver = TestSolver::default();
        let c = solver.new_variable(4, 26);
        let d = solver.new_variable(13, 13);
        let e = solver.new_variable(5, 10);
        let f = solver.new_variable(5, 10);

        let _ = solver
            .new_propagator(Disjunctive::new([
                ArgDisjunctiveTask {
                    start_variable: c,
                    processing_time: 4,
                },
                ArgDisjunctiveTask {
                    start_variable: d,
                    processing_time: 5,
                },
                ArgDisjunctiveTask {
                    start_variable: e,
                    processing_time: 3,
                },
                ArgDisjunctiveTask {
                    start_variable: f,
                    processing_time: 3,
                },
            ]))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(c), 18);
    }
}
