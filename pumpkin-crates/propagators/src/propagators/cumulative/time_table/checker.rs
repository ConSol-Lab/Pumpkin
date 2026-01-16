use std::collections::BTreeMap;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;

#[derive(Clone, Debug)]
pub struct TimeTableChecker<Var> {
    pub tasks: Box<[CheckerTask<Var>]>,
    pub capacity: i32,
}

#[derive(Clone, Debug)]
pub struct CheckerTask<Var> {
    pub start_time: Var,
    pub resource_usage: i32,
    pub duration: i32,
}

impl<Var, Atomic> InferenceChecker<Atomic> for TimeTableChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(&self, state: pumpkin_checking::VariableState<Atomic>) -> bool {
        // The profile is a key-value store. The keys correspond to time-points, and the values to
        // the relative change in resource consumption. A BTreeMap is used to maintain a
        // sorted order of the time points.
        let mut profile = BTreeMap::new();

        for task in self.tasks.iter() {
            let lst = task.start_time.induced_upper_bound(&state);
            let ect = task.start_time.induced_lower_bound(&state) + task.duration;

            if ect <= lst {
                *profile.entry(ect).or_insert(0) += task.resource_usage;
                *profile.entry(lst).or_insert(0) -= task.resource_usage;
            }
        }

        let mut usage = 0;
        for delta in profile.values() {
            usage += delta;

            if usage > self.capacity {
                return true;
            }
        }

        false
    }
}
