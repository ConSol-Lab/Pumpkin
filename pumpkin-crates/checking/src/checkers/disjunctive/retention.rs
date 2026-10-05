use std::cmp::Reverse;

use super::DisjunctiveEdgeFindingChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::RetentionCheck;
use crate::RetentionChecker;
use crate::VariableState;

/// A task with the bounds it has in the state.
#[derive(Clone, Copy, Debug)]
struct BoundedTask {
    /// The earliest start time.
    est: i64,
    /// The latest completion time.
    lct: i64,
    processing_time: i64,
}

/// Mirrors one pass of the edge-finding propagator, which only propagates lower bounds.
///
/// The propagator goes over the sets `Θ_k` of the tasks from position `k` on, in non-increasing
/// order of latest completion time, and `Λ_k` of the tasks before position `k`. It has nothing left
/// to propagate when
/// - no `Θ_k`, with `k` before the last position, overloads its latest completion time `lct_k`,
///   i.e. `ECT(Θ_k) <= lct_k`, and
/// - for every `k` after the first position and every task `i` in `Λ_k` with `ECT(Θ_k ∪ {i}) >
///   lct_k`, the earliest start time of `i` is at least `ECT(Θ_k)`.
///
/// The earliest completion time of a set is `max` over its subsets `Ω` of `est_Ω + p_Ω`.
impl<Var, Atomic> RetentionChecker<Atomic> for DisjunctiveEdgeFindingChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check_retention(&self, state: &VariableState<Atomic>) -> RetentionCheck {
        let mut tasks = self
            .tasks
            .iter()
            .map(|task| {
                let est: i32 = task
                    .start_time
                    .induced_lower_bound(state)
                    .try_into()
                    .expect("the domains of a retention check are bounded");
                let lst: i32 = task
                    .start_time
                    .induced_upper_bound(state)
                    .try_into()
                    .expect("the domains of a retention check are bounded");
                BoundedTask {
                    est: i64::from(est),
                    lct: i64::from(lst) + i64::from(task.processing_time),
                    processing_time: i64::from(task.processing_time),
                }
            })
            .collect::<Vec<_>>();

        // The order among tasks with the same latest completion time does not change what the
        // propagator derives.
        tasks.sort_by_key(|task| Reverse(task.lct));

        for k in 0..tasks.len() {
            let (lambda, theta) = tasks.split_at(k);
            let lct_k = theta[0].lct;
            let ect_theta = earliest_completion_time(theta.iter());

            if k + 1 < tasks.len() && ect_theta > lct_k {
                log::error!(
                    "The tasks {theta:?} cannot all complete by {lct_k}; the overload should have been reported as a conflict"
                );
                return RetentionCheck::PropagationMissed;
            }

            for task in lambda {
                if task.est < ect_theta
                    && earliest_completion_time(theta.iter().chain(std::iter::once(task))) > lct_k
                {
                    log::error!(
                        "The earliest start time of {task:?} could be raised to {ect_theta}: it cannot complete before the tasks {theta:?}"
                    );
                    return RetentionCheck::PropagationMissed;
                }
            }
        }

        RetentionCheck::NothingToPropagate
    }
}

/// The earliest completion time of the tasks: the greatest `est_Ω + p_Ω` over the subsets `Ω`,
/// which is reached by a set of the tasks that start no earlier than one of them.
fn earliest_completion_time<'a>(tasks: impl Iterator<Item = &'a BoundedTask>) -> i64 {
    let mut tasks = tasks.collect::<Vec<_>>();
    tasks.sort_by_key(|task| Reverse(task.est));

    let mut processing_time_after = 0;
    let mut ect = i64::MIN;
    for task in tasks {
        processing_time_after += task.processing_time;
        ect = ect.max(task.est + processing_time_after);
    }
    ect
}
