use std::cmp::Reverse;
use std::cmp::min;

use super::disjunctive_task::ArgDisjunctiveTask;
use super::disjunctive_task::DisjunctiveTask;
use super::theta_lambda_tree::ThetaLambdaTree;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::containers::StorageKey;
use crate::predicate;
use crate::predicates::PropositionalConjunction;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::DomainEvents;
use crate::propagation::LocalId;
use crate::propagation::PropagationContextMut;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::propagators::disjunctive::DisjunctiveEdgeFinding;
use crate::pumpkin_assert_simple;
use crate::state::Conflict;
use crate::variables::IntegerVariable;

/// [`Propagator`] responsible for using disjunctive reasoning to propagate the [Disjunctive](https://sofdem.github.io/gccat/gccat/Cdisjunctive.html) constraint.
///
/// Currently, this propagator only implements edge-finding as specified in \[1\] with explanations
/// based on \[2\]. The reasoning of this approach is based on finding a task i and a subset of
/// tasks for which it holds that if we were to schedule i at its earliest start time then it would
/// overflow the resource capacity and thus i should be scheduled after all activities from this
/// set.
///
/// It follows the [MiniZinc specifications](https://docs.minizinc.dev/en/stable/lib-globals-scheduling.html#mzn-ref-globals-scheduling-disjunctive-strict) which means that tasks with duration 0 can only be scheduled when no other tasks are running.
///
/// Note: This propagator only performs lower-bound propagation (though an analogous propagator for
/// upper-bounds can be achieved using views).
///
/// # Bibliography
/// - \[1\] P. Vilím, ‘Filtering algorithms for the unary resource constraint’, Archives of Control
///   Sciences, vol. 18, no. 2, pp. 159–202, 2008.
/// - \[2\] R. A. Vasile, ‘Evaluating the Impact of Explanations on the Performance of an
///   Edge-Finding Propagator’.
#[derive(Debug, Clone)]
pub(crate) struct DisjunctivePropagator<Var: IntegerVariable> {
    /// The tasks which serve as the input to the disjunctive constraint
    tasks: Box<[DisjunctiveTask<Var>]>,
    /// An additional list of tasks which allows us to sort them (we require [`Disjunctive::tasks`]
    /// to keep track of the right indices).
    sorted_tasks: Vec<DisjunctiveTask<Var>>,
    /// The theta-lambda tree used to calculate the earliest completion time of a set of tasks.
    ///
    /// For an explanation of how it is used, see the documentation and \[1\].
    theta_lambda_tree: ThetaLambdaTree<Var>,

    inference_code: InferenceCode,
}

pub(crate) struct DisjunctiveConstructor<Var> {
    constraint_tag: ConstraintTag,
    tasks: Vec<ArgDisjunctiveTask<Var>>,
}

impl<Var> DisjunctiveConstructor<Var> {
    pub(crate) fn new(
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

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
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

        let inference_code =
            context.create_inference_code(self.constraint_tag, DisjunctiveEdgeFinding);

        tasks.iter().for_each(|task| {
            context.register(task.start_time.clone(), DomainEvents::BOUNDS, task.id);
        });

        DisjunctivePropagator {
            tasks: tasks.clone().into_boxed_slice(),
            sorted_tasks: tasks,
            theta_lambda_tree,

            inference_code,
        }
    }
}

impl<Var: IntegerVariable + 'static> Propagator for DisjunctivePropagator<Var> {
    fn name(&self) -> &str {
        "DisjunctiveStrict"
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        edge_finding(
            &mut self.theta_lambda_tree,
            &mut context,
            &self.tasks,
            &mut self.sorted_tasks,
            self.inference_code,
        )
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let mut sorted_tasks = self.sorted_tasks.clone();
        let mut theta_lambda_tree = self.theta_lambda_tree.clone();
        edge_finding(
            &mut theta_lambda_tree,
            &mut context,
            &self.tasks,
            &mut sorted_tasks,
            self.inference_code,
        )
    }
}

/// Performs the edge-finding algorithm (see [`Disjunctive`] for an intuition and the work on which
/// this implementation is based).
fn edge_finding<Var: IntegerVariable, SortedTaskVar: IntegerVariable>(
    theta_lambda_tree: &mut ThetaLambdaTree<Var>,
    context: &mut PropagationContextMut,
    tasks: &[DisjunctiveTask<Var>],
    sorted_tasks: &mut [DisjunctiveTask<SortedTaskVar>],
    inference_code: InferenceCode,
) -> PropagationStatusCP {
    // First we create our Theta-Lambda tree and add all of the tasks to Theta (Lambda is empty at
    // this point)
    theta_lambda_tree.update(context.domains());
    for task in tasks.iter() {
        theta_lambda_tree.add_to_theta(task, context.domains());
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
            return Err(Conflict::Propagator(PropagatorConflict {
                conjunction: create_conflict_explanation(theta_lambda_tree, context, lct_j),
                inference_code,
            }));
        }

        // If there was no overflow then we continue by checking whether we can find a propagation
        // from the tasks

        // To do this, we first remove the task from Theta
        theta_lambda_tree.remove_from_theta(j);
        // And then add it to Lambda (i.e. we are checking whether we can find a task i in Lambda
        // such that the element in Theta would cause an overflow)
        theta_lambda_tree.add_to_lambda(j, context.domains());

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
                            theta_lambda_tree,
                            context,
                            new_bound,
                            lct_j,
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

/// Creates an explanation consisting of the tasks in the theta-lambda tree which were responsible
/// for the conflict based on \[1\] and \[2\].
///
/// # Bibliography
/// - \[1\] P. Vilím, ‘Computing explanations for the unary resource constraint’, in International
///   Conference on Integration of Artificial Intelligence (AI) and Operations Research (OR)
///   Techniques in Constraint Programming, 2005, pp. 396–409.
/// - \[2\] R. A. Vasile, ‘Evaluating the Impact of Explanations on the Performance of an
///   Edge-Finding Propagator’.
fn create_conflict_explanation<Var: IntegerVariable>(
    theta_lambda_tree: &mut ThetaLambdaTree<Var>,
    context: &PropagationContextMut,
    lct: i32,
) -> PropositionalConjunction {
    // We get the set of tasks currently in theta
    let theta = theta_lambda_tree.get_theta();

    pumpkin_assert_simple!(!theta.is_empty());

    // Recall that we want to find the set omega such that:
    // `p_omega > lct_omega - est_omega`
    //
    // We first assume that theta = omega
    let mut est = context.lower_bound(&theta[0].start_time);
    let mut p_omega = theta_lambda_tree.sum_of_processing_times();

    // And we track whether we still satisfy this value
    let mut delta = p_omega - (lct - est) - 1;

    let mut i = 0;

    while i < theta.len() - 1 {
        // If this holds then we have found our set omega
        if delta >= 0 {
            break;
        }
        let task = &theta[i];

        // Otherwise we remove the task i and continue
        p_omega -= task.processing_time;
        // Note the `i + 1` here!
        est = context.lower_bound(&theta[i + 1].start_time);
        delta = p_omega - (lct - est) - 1;

        i += 1;
    }

    // We use an overflow in the explanation to generalise it
    let offset_left = (delta as f64 / 2.0).floor() as i32;
    let offset_right = (delta as f64 / 2.0).ceil() as i32;

    let mut explanation = Vec::new();

    for task in theta.iter().skip(i) {
        explanation.push(predicate!(task.start_time >= est - offset_left));
        explanation.push(predicate!(
            task.start_time <= lct + offset_right - task.processing_time
        ))
    }

    explanation.into()
}

/// Creates an explanation consisting of the tasks in the theta-lambda tree which were responsible
/// for the propagation of `propagated_task` based on \[1\] and \[2\].
///
/// # Bibliography
/// - \[1\] P. Vilím, ‘Computing explanations for the unary resource constraint’, in International
///   Conference on Integration of Artificial Intelligence (AI) and Operations Research (OR)
///   Techniques in Constraint Programming, 2005, pp. 396–409.
/// - \[2\] R. A. Vasile, ‘Evaluating the Impact of Explanations on the Performance of an
///   Edge-Finding Propagator’.
fn create_propagation_explanation<'a, Var: IntegerVariable>(
    original_tasks: &'a [DisjunctiveTask<Var>],
    propagated_task_id: LocalId,
    theta_lambda_tree: &mut ThetaLambdaTree<Var>,
    context: &'a PropagationContextMut,
    new_bound: i32,
    lct_j: i32,
) -> PropositionalConjunction {
    // We get the set of tasks currently in theta
    let theta = theta_lambda_tree.get_theta();

    pumpkin_assert_simple!(!theta.is_empty());

    // Recall that i has to be scheduled after a set omega if the following holds:
    // `min(est_i, est_omega) + p_omega + p_i > lct_omega`
    //
    // First we retrieve some information about i
    let propagated_task = &original_tasks[propagated_task_id.index()];
    let est_propagated = context.lower_bound(&propagated_task.start_time);

    // Then we get some information about omega
    let mut p_omega = theta_lambda_tree.sum_of_processing_times();

    // Recall that the value for the precedence is only based on the earliest start time (and
    // lct_omega is fixed at the time of the propagation).
    //
    // This means that we can remove tasks while the precedence relation is still implied by omega.
    let mut i = 0;
    // We keep track of the value of the current set.
    let mut delta = min(est_propagated, context.lower_bound(&theta[i].start_time))
        + propagated_task.processing_time
        + p_omega;

    // Thus, we keep increasing i until we know that the precedence relation does not hold.
    while i < theta.len() && delta <= lct_j {
        // We remove i from omega and continue
        p_omega -= theta[i].processing_time;
        i += 1;
        if i == theta.len() {
            break;
        }
        // Now we update the value of the left-hand side of the equation such that omega is bounded
        // by i on the lower side
        delta = min(est_propagated, context.lower_bound(&theta[i].start_time))
            + p_omega
            + propagated_task.processing_time;
    }

    pumpkin_assert_simple!(i < theta.len());

    // Now we want to find the set omega prime which caused the actual bound update to happen.
    //
    // We thus search for the set omega_prime such that `theta-lambda-tree.ect() ==
    // est_{omega_prime} + p_{omega_prime}`.
    //
    // Since this is only dependent on the est of omega_prime, we will iterate over the tasks
    // sorted in non-decreasing est.

    // We start with omega_prime = omega
    let mut j = i;
    let mut p_omega_prime = p_omega;
    while j < theta.len() {
        if theta_lambda_tree.ect() == context.lower_bound(&theta[j].start_time) + p_omega_prime {
            // If we have found the correct set then we can break
            break;
        }

        // Otherwise we remove j from omega_prime
        p_omega_prime -= theta[j].processing_time;
        j += 1;
    }

    pumpkin_assert_simple!(j < theta.len());

    let mut explanation = Vec::new();

    // Now we calculate the explanation
    let r = min(est_propagated, context.lower_bound(&theta[i].start_time));
    for (task_index, task) in theta.iter().enumerate().skip(i) {
        if task_index < j {
            // Element is part of omega
            explanation.push(predicate!(task.start_time >= r));
        } else {
            // Element is part of omega_prime
            explanation.push(predicate!(task.start_time >= new_bound - p_omega_prime));
        }

        // We also constrain the upper-bound
        explanation.push(predicate!(
            task.start_time
                <= r + p_omega + propagated_task.processing_time - 1 - task.processing_time
        ))
    }
    explanation.push(predicate!(propagated_task.start_time >= r));

    explanation.into()
}

#[cfg(test)]
mod tests {
    use crate::engine::test_solver::TestSolver;
    use crate::propagators::disjunctive_propagator::DisjunctiveConstructor;
    use crate::propagators::disjunctive_task::ArgDisjunctiveTask;

    #[test]
    fn propagator_propagates_lower_bound() {
        let mut solver = TestSolver::default();
        let c = solver.new_variable(4, 26);
        let d = solver.new_variable(13, 13);
        let e = solver.new_variable(5, 10);
        let f = solver.new_variable(5, 10);

        let constraint_tag = solver.new_constraint_tag();
        let _ = solver
            .new_propagator(DisjunctiveConstructor::new(
                [
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
                ],
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(c), 18);
    }
}
