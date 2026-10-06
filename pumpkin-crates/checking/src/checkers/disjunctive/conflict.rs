use super::DisjunctiveCheckerTask;
use super::DisjunctiveEdgeFindingChecker;
use super::helpers::CheckerThetaLambdaTree;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::ConflictCheck;
use crate::ConflictChecker;
use crate::IntExt;
use crate::VariableState;

/// Whether overload checking finds a set `omega` of the tasks with finite bounds such that
/// `p_omega > lct_omega - est_omega`.
fn overload_checking<Atomic: AtomicConstraint, Var: CheckerVariable<Atomic>>(
    tasks: &[DisjunctiveCheckerTask<Var>],
    state: &VariableState<Atomic>,
) -> bool {
    let mut theta = CheckerThetaLambdaTree::new(tasks);
    theta.update(state);

    // The tasks with finite bounds, in non-decreasing order of latest completion time.
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

    for (index, task) in sorted_tasks {
        debug_assert!(
            task.start_time.induced_lower_bound(state) != IntExt::NegativeInf
                && task.start_time.induced_upper_bound(state) != IntExt::PositiveInf
        );
        theta.add_to_theta(index, task, state);

        // Theta cannot complete by the latest completion time of `task`: an overload.
        if theta.ect() > task.start_time.induced_upper_bound(state) + task.processing_time {
            return true;
        }
    }

    false
}

impl<Var, Atomic> ConflictChecker<Atomic> for DisjunctiveEdgeFindingChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
    <Atomic as AtomicConstraint>::Identifier: Clone,
{
    fn check(
        &self,
        state: &VariableState<Atomic>,
        _premises: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> ConflictCheck {
        // Two cases:
        // 1. If it is a conflict explanation then overload checking can be applied directly and
        //    should lead to a conflict.
        // 2. If it is a propagation explanation, then for any value in the domain of the propagated
        //    variable, scheduling it at that time-point should lead to a conflict (using overload
        //    checking).
        if let Some(consequent) = consequent {
            let task = self
                .tasks
                .iter()
                .find(|task| task.start_time.does_atomic_constrain_self(consequent))
                .expect("Expected to be able to find atomic");

            let lb: i32 = task
                .start_time
                .induced_lower_bound(state)
                .try_into()
                .expect("expected non-infinity value");
            let ub: i32 = task
                .start_time
                .induced_upper_bound(state)
                .try_into()
                .expect("expected non-infinity value");

            for i in lb..=ub {
                let mut assigned_state = state.clone();
                let _ = assigned_state.apply(&task.start_time.atomic_equal(i));

                if !overload_checking(&self.tasks, &assigned_state) {
                    return ConflictCheck::NoConflictDetected;
                }
            }
            ConflictCheck::ConflictDetected
        } else if overload_checking(&self.tasks, state) {
            ConflictCheck::ConflictDetected
        } else {
            ConflictCheck::NoConflictDetected
        }
    }
}
