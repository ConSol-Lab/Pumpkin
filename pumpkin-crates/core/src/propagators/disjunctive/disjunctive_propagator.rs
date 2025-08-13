use std::cmp::Reverse;

use fixedbitset::FixedBitSet;

use super::disjunctive_task::ArgDisjunctiveTask;
use super::disjunctive_task::DisjunctiveTask;
use super::theta_lambda_tree::ThetaLambdaTree;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::containers::StorageKey;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::DomainEvents;
use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagators::disjunctive::DisjunctiveEdgeFinding;
use crate::variables::IntegerVariable;
use crate::variables::TransformableVariable;

/// [`Propagator`] responsible for using disjunctive reasoning to propagate the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) constraint.
///
/// Currently, this propagator only implements edge-finding as specified in \[1\]. The reasoning of
/// this approach is based on finding a task i and a subset of tasks for which it holds that if we
/// were to schedule i at its earliest start time then it would overflow the resource capacity and
/// thus i should be scheduled after all activities from this set.
///
/// # Bibliography
/// \[1\] P. Vilím, ‘Filtering algorithms for the unary resource constraint’, Archives of Control
/// Sciences, vol. 18, no. 2, pp. 159–202, 2008.
pub(crate) struct DisjunctivePropagator<Var: IntegerVariable> {
    /// The tasks which serve as the input to the disjunctive constraint
    tasks: Box<[DisjunctiveTask<Var>]>,
    /// An additional list of tasks which allows us to sort them (we require [`Disjunctive::tasks`]
    /// to keep track of the right indices).
    sorted_tasks: Vec<DisjunctiveTask<Var>>,
    /// The tasks which are used to propagate the upper-bounds; these are the same as
    /// [`Disjunctive::tasks`] but reversed (i.e. instead of being from [EST, LCT] these tasks go
    /// from [-LCT, -EST])
    reverse_tasks: Box<
        [DisjunctiveTask<<<Var as IntegerVariable>::AffineView as IntegerVariable>::AffineView>],
    >,
    /// An additional list of tasks which allows us to sort them (we require
    /// [`Disjunctive::reverse_tasks`]
    /// to keep track of the right indices).
    sorted_reverse_tasks:
        Vec<DisjunctiveTask<<<Var as IntegerVariable>::AffineView as IntegerVariable>::AffineView>>,

    /// The elements which are currently present in the set Theta used for edge-finding.
    elements_in_theta: FixedBitSet,

    constraint_tag: ConstraintTag,
    inference_code: Option<InferenceCode>,
}

impl<Var: IntegerVariable + 'static> PropagatorConstructor for DisjunctivePropagator<Var> {
    type PropagatorImpl = Self;

    fn create(mut self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        self.inference_code =
            Some(context.create_inference_code(self.constraint_tag, DisjunctiveEdgeFinding));

        self.tasks.iter().for_each(|task| {
            context.register(task.start_time.clone(), DomainEvents::BOUNDS, task.id);
        });

        self
    }
}

impl<Var: IntegerVariable + 'static> DisjunctivePropagator<Var> {
    pub(crate) fn new(
        tasks: impl IntoIterator<Item = ArgDisjunctiveTask<Var>>,
        constraint_tag: ConstraintTag,
    ) -> Self {
        let tasks = tasks
            .into_iter()
            .enumerate()
            .map(|(index, task)| DisjunctiveTask {
                start_time: task.start_time.clone(),
                processing_time: task.processing_time,
                id: LocalId::from(index as u32),
            })
            .collect::<Vec<_>>();
        let reverse_tasks = tasks
            .iter()
            .map(|task| DisjunctiveTask {
                start_time: task.start_time.offset(task.processing_time).scaled(-1),
                processing_time: task.processing_time,
                id: task.id,
            })
            .collect::<Vec<_>>();

        let num_tasks = tasks.len();

        Self {
            tasks: tasks.clone().into_boxed_slice(),
            sorted_tasks: tasks,
            reverse_tasks: reverse_tasks.clone().into_boxed_slice(),
            sorted_reverse_tasks: reverse_tasks,
            elements_in_theta: FixedBitSet::with_capacity(num_tasks),
            constraint_tag,
            inference_code: None,
        }
    }
}

/// Creates an explanation consisting of the tasks in the theta-lambda tree which were responsible
/// for the conflict based on \[1\].
///
/// # Bibliography
/// \[1\] P. Vilím, ‘Computing explanations for the unary resource constraint’, in International
/// Conference on Integration of Artificial Intelligence (AI) and Operations Research (OR)
/// Techniques in Constraint Programming, 2005, pp. 396–409.
fn create_conflict_explanation<'a, Var: IntegerVariable>(
    tasks: &'a [DisjunctiveTask<Var>],
    _theta_lambda_tree: &mut ThetaLambdaTree<Var>,
    context: &'a PropagationContextMut,
    elements_in_theta: &FixedBitSet,
) -> PropositionalConjunction {
    let mut explanation = Vec::new();

    for task_index in elements_in_theta.ones() {
        let task = &tasks[task_index];

        explanation.push(predicate!(
            task.start_time >= context.lower_bound(&task.start_time)
        ));
        explanation.push(predicate!(
            task.start_time <= context.upper_bound(&task.start_time)
        ));
    }

    explanation.into()
}

/// Creates an explanation consisting of the tasks in the theta-lambda tree which were responsible
/// for the propagation of `propagated_task` based on \[1\].
///
/// # Bibliography
/// \[1\] P. Vilím, ‘Computing explanations for the unary resource constraint’, in International
/// Conference on Integration of Artificial Intelligence (AI) and Operations Research (OR)
/// Techniques in Constraint Programming, 2005, pp. 396–409.
fn create_propagation_explanation<'a, Var: IntegerVariable>(
    tasks: &'a [DisjunctiveTask<Var>],
    propagated_task_id: LocalId,
    _theta_lambda_tree: &mut ThetaLambdaTree<Var>,
    context: &'a PropagationContextMut,
    elements_in_theta: &mut FixedBitSet,
) -> PropositionalConjunction {
    let mut explanation = Vec::new();

    for task_index in elements_in_theta.ones() {
        let task = &tasks[task_index];

        explanation.push(predicate!(
            task.start_time >= context.lower_bound(&task.start_time)
        ));
        explanation.push(predicate!(
            task.start_time <= context.upper_bound(&task.start_time)
        ));
    }

    let propagated_task = &tasks[propagated_task_id.index()];
    explanation.push(predicate!(
        propagated_task.start_time >= context.lower_bound(&propagated_task.start_time)
    ));
    explanation.push(predicate!(
        propagated_task.start_time <= context.upper_bound(&propagated_task.start_time)
    ));

    explanation.into()
}

/// Performs the edge-finding algorithm (see [`Disjunctive`] for an intuition and the work on which
/// this implementation is based).
fn edge_finding<Var: IntegerVariable, SortedTaskVar: IntegerVariable>(
    context: &mut PropagationContextMut,
    tasks: &[DisjunctiveTask<Var>],
    sorted_tasks: &mut [DisjunctiveTask<SortedTaskVar>],
    elements_in_theta: &mut FixedBitSet,
    inference_code: InferenceCode,
) -> PropagationStatusCP {
    // First we create our Theta-Lambda tree and add all of the tasks to Theta (Lambda is empty at
    // this point)
    let mut theta_lambda_tree = ThetaLambdaTree::new(tasks, context.as_readonly());
    for task in tasks.iter() {
        elements_in_theta.insert(task.id.index());
        theta_lambda_tree.add_to_theta(task, context.as_readonly());
    }

    // Then sort in non-increasing order of latest completion time (LCT)
    sorted_tasks
        .sort_by_key(|task| Reverse(context.upper_bound(&task.start_time) + task.processing_time));

    // Then we get the first element from the lambda tree with the highest value of LCT
    let mut index = 0;
    let mut j = &sorted_tasks[index];
    let mut lct_j = context.upper_bound(&j.start_time) + j.processing_time;

    // While we have elements in theta, we keep iterating the algorithm
    while index < tasks.len() - 1 {
        // We know that `j` represents the element in Theta with the highest LCT, if the ECT
        // (which takes into account `j`) is larger than the LCT of `j` then we can report an
        // overflow
        if theta_lambda_tree.ect() > lct_j {
            return Err(Inconsistency::Conflict(PropagatorConflict {
                conjunction: create_conflict_explanation(
                    tasks,
                    &mut theta_lambda_tree,
                    context,
                    elements_in_theta,
                ),
                inference_code,
            }));
        }

        // If there was no overflow then we continue by checking whether we can find a propagation
        // from the tasks

        // To do this, we first remove the task from Theta
        theta_lambda_tree.remove_from_theta(j);
        elements_in_theta.remove(j.id.index());
        // And then add it to Lambda (i.e. we are checking whether we can find a task i in Lambda
        // such that the element in Theta would cause an overflow)
        theta_lambda_tree.add_to_lambda(j, context.as_readonly());

        // Then we go to the next task which represents the latest completion time of the set Theta
        index += 1;
        j = &sorted_tasks[index];
        lct_j = context.upper_bound(&j.start_time) + j.processing_time;

        // Then we try to find tasks in Lambda such that the edge-finding condition holds
        //
        // i.e. Find an element such that `ECT_{i union Theta} > lct_j`
        while theta_lambda_tree.ect_bar() > lct_j {
            // We know that the condition holds and now we need to retrieve the element in Lambda
            // which was responsible for the condition holding
            if let Some(i) = theta_lambda_tree.responsible_ect_bar() {
                // We calculate the new bound
                let new_bound = theta_lambda_tree.ect();

                // Then we check whether we can update; if this is the case then we set the new
                // lower-bound to be after `ECT_{Theta}`
                if new_bound > context.lower_bound(&tasks[i.index()].start_time) {
                    // Propagate
                    let propagated_variable = &tasks[i.index()].start_time;
                    let propagated_predicate = predicate!(propagated_variable >= new_bound);
                    context.post(
                        propagated_predicate,
                        create_propagation_explanation(
                            tasks,
                            i,
                            &mut theta_lambda_tree,
                            context,
                            elements_in_theta,
                        ),
                        inference_code,
                    )?;
                }

                // Then we remove the element from consideration entirely by removing it from Lambda
                // and continue to see if there are other elements from Lambda which could be
                // updated
                theta_lambda_tree.remove_from_lambda(&tasks[i.index()]);
            } else {
                break;
            }
        }
    }

    Ok(())
}

impl<Var: IntegerVariable + 'static> Propagator for DisjunctivePropagator<Var> {
    fn name(&self) -> &str {
        "Disjunctive"
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        // First we perform a "regular" round of edge-finding updating the lower-bounds
        edge_finding(
            &mut context,
            &self.tasks,
            &mut self.sorted_tasks,
            &mut self.elements_in_theta,
            self.inference_code.unwrap(),
        )?;

        // Now we want to also update the upper-bounds
        //
        // We do this by "reversing" the tasks to make the LCT be represented as the EST of a task
        edge_finding(
            &mut context,
            &self.reverse_tasks,
            &mut self.sorted_reverse_tasks,
            &mut self.elements_in_theta,
            self.inference_code.unwrap(),
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
            self.inference_code.unwrap(),
        )?;

        let mut sorted_reverse_tasks = self.sorted_reverse_tasks.clone();
        edge_finding(
            &mut context,
            &self.reverse_tasks,
            &mut sorted_reverse_tasks,
            &mut elements_in_theta,
            self.inference_code.unwrap(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::test_solver::TestSolver;
    use crate::propagators::disjunctive::disjunctive_propagator::DisjunctivePropagator;
    use crate::propagators::disjunctive_task::ArgDisjunctiveTask;

    #[test]
    fn propagator_propagates_lower_bound() {
        let mut solver = TestSolver::default();
        let c = solver.new_variable(4, 26);
        let d = solver.new_variable(13, 13);
        let e = solver.new_variable(5, 10);
        let f = solver.new_variable(5, 10);

        let _ = solver
            .new_propagator(DisjunctivePropagator::new([
                ArgDisjunctiveTask {
                    start_time: c,
                    processing_time: 4,
                },
                ArgDisjunctiveTask {
                    start_time: d,
                    processing_time: 5,
                },
                ArgDisjunctiveTask {
                    start_time: e,
                    processing_time: 3,
                },
                ArgDisjunctiveTask {
                    start_time: f,
                    processing_time: 3,
                },
            ]))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(c), 18);
    }
}
