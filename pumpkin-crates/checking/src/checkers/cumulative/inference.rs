use std::collections::BTreeMap;

use super::CheckerTask;
use super::TimeTableChecker;
use crate::AtomicConstraint;
use crate::CheckerVariable;
use crate::InferenceChecker;
use crate::IntExt;
use crate::VariableState;

fn can_be_propagated_by_profile<Var: CheckerVariable<Atomic>, Atomic: AtomicConstraint>(
    task: &CheckerTask<Var>,
    height: i32,
    capacity: i32,
) -> bool {
    height + task.resource_usage > capacity
}

impl<Var, Atomic> InferenceChecker<Atomic> for TimeTableChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(
        &self,
        mut state: VariableState<Atomic>,
        _: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        // The profile is a key-value store. The keys correspond to time-points, and the values to
        // the relative change in resource consumption. A BTreeMap is used to maintain a
        // sorted order of the time points.
        let mut profile = BTreeMap::new();

        for task in self.tasks.iter() {
            if task.resource_usage > self.capacity {
                return true;
            }
            if task.start_time.induced_lower_bound(&state) == IntExt::NegativeInf
                || task.start_time.induced_upper_bound(&state) == IntExt::PositiveInf
            {
                continue;
            }
            let lst: i32 = task
                .start_time
                .induced_upper_bound(&state)
                .try_into()
                .unwrap();
            let est: i32 = task
                .start_time
                .induced_lower_bound(&state)
                .try_into()
                .unwrap();

            for t in lst..est + task.processing_time {
                *profile.entry(t).or_insert(0) += task.resource_usage;
                if *profile.get(&t).unwrap() > self.capacity {
                    return true;
                }
            }
        }

        if let Some(propagating_task) = consequent.map(|consequent| {
            self.tasks
                .iter()
                .find(|task| task.start_time.does_atomic_constrain_self(consequent))
                .expect("If there is a consequent, then there should be a propagating task")
        }) {
            let lst: i32 = propagating_task
                .start_time
                .induced_upper_bound(&state)
                .try_into()
                .unwrap();
            let est: i32 = propagating_task
                .start_time
                .induced_lower_bound(&state)
                .try_into()
                .unwrap();

            for t in lst..est + propagating_task.processing_time {
                *profile.entry(t).or_insert(0) -= propagating_task.resource_usage;
                if *profile.get(&t).unwrap() > self.capacity {
                    return true;
                }
            }

            for (t, height) in profile.iter() {
                if can_be_propagated_by_profile(propagating_task, *height, self.capacity) {
                    for t in (t - propagating_task.processing_time + 1)..=*t {
                        if !state.apply(&propagating_task.start_time.atomic_not_equal(t)) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}
