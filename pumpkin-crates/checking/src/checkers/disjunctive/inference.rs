use super::DisjunctiveCheckerTask;
use super::DisjunctiveEdgeFindingChecker;
use super::helpers::CheckerThetaLambdaTree;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::IntExt;
use crate::VariableState;

/// Performs overload checking on the provided `tasks`
/// and returns true if a conflict could be found.
///
/// Recall the following:
/// We try to find a set omega of jobs with the following property:
/// `p_omega > lct_omega - est_omega`.
fn overload_checking<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    tasks: &[DisjunctiveCheckerTask<Var>],
    state: &VariableState<Atomic>,
) -> bool {
    // First, we create our theta-lambda tree
    let mut theta = CheckerThetaLambdaTree::new(tasks);
    // And update it with the current state.
    theta.update(state);

    // Next, we sort based on non-decreasing latest completion time.
    let mut sorted_tasks = tasks
        .iter()
        .enumerate()
        .filter(|(_, task)| {
            task.start_time.induced_lower_bound(state) != IntExt::NegativeInf
                && task.start_time.induced_upper_bound(state) != IntExt::PositiveInf
        })
        .collect::<Vec<_>>();
    sorted_tasks
        .sort_by_key(|(_, task)| task.start_time.induced_upper_bound(state) + task.processing_time);

    // Then we go over the tasks which are bounded in the state.
    for (index, task) in sorted_tasks {
        debug_assert!(
            task.start_time.induced_lower_bound(state) != IntExt::NegativeInf
                && task.start_time.induced_upper_bound(state) != IntExt::PositiveInf
        );
        // And we add it to the theta.
        theta.add_to_theta(index, task, state);

        // If there is an overload of the interval,
        // then we can report that a conflict has been found.
        if theta.ect() > task.start_time.induced_upper_bound(state) + task.processing_time {
            return true;
        }
    }

    false
}

impl<Var, Atomic> InferenceChecker<Atomic> for DisjunctiveEdgeFindingChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
    <Atomic as AtomicConstraint>::Identifier: Clone,
{
    fn check(
        &self,
        state: VariableState<Atomic>,
        _premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        // We want to detect conflicts, and we split into two cases:
        // 1. If it is a conflict explanation then overload checking can be applied directly and
        //    should lead to a conflict.
        // 2. If it is a propagation explanation, then for any value in the domain of the propagated
        //    variable, scheduling it at that time-point should lead to a conflict (using overload
        //    checking).
        if let Some(consequent) = consequent {
            // First we retrieve the propagating task.
            let task = self
                .tasks
                .iter()
                .find(|task| task.start_time.does_atomic_constrain_self(consequent))
                .expect("Expected to be able to find atomic");

            let lb: i32 = task
                .start_time
                .induced_lower_bound(&state)
                .try_into()
                .expect("expected non-infinity value");
            let ub: i32 = task
                .start_time
                .induced_upper_bound(&state)
                .try_into()
                .expect("expected non-infinity value");

            // Then we go over every value in its domain.
            for i in lb..=ub {
                // We assign the propagating variable to that value.
                let mut assigned_state = state.clone();
                let _ = assigned_state.apply(&task.start_time.atomic_equal(i));

                // If we do not find a conflict using overload checking,
                // then it is not a valid explanation.
                if !overload_checking(&self.tasks, &assigned_state) {
                    return false;
                }
            }
            true
        } else {
            overload_checking(&self.tasks, &state)
        }
    }
}
