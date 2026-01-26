use std::collections::BTreeMap;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

use crate::cumulative::time_table::time_table_util::has_overlap_with_interval;

#[derive(Clone, Debug)]
pub struct TimeTableChecker<Var> {
    pub tasks: Box<[CheckerTask<Var>]>,
    pub capacity: i32,
}

#[derive(Clone, Debug)]
pub struct CheckerTask<Var> {
    pub start_time: Var,
    pub resource_usage: i32,
    pub processing_time: i32,
}

fn lower_bound_can_be_propagated_by_profile<
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
>(
    context: &VariableState<Atomic>,
    lower_bound: i32,
    task: &CheckerTask<Var>,
    start: i32,
    end: i32,
    height: i32,
    capacity: i32,
) -> bool {
    let upper_bound = task
        .start_time
        .induced_upper_bound(context)
        .try_into()
        .unwrap();

    height + task.resource_usage > capacity
        && !(upper_bound < (lower_bound + task.processing_time)
            && has_overlap_with_interval(
                upper_bound,
                lower_bound + task.processing_time,
                start,
                end,
            ))
        && has_overlap_with_interval(lower_bound, upper_bound + task.processing_time, start, end)
        && (lower_bound + task.processing_time) > start
        && lower_bound <= end
}

fn upper_bound_can_be_propagated_by_profile<
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
>(
    context: &VariableState<Atomic>,
    upper_bound: i32,
    task: &CheckerTask<Var>,
    start: i32,
    end: i32,
    height: i32,
    capacity: i32,
) -> bool {
    let lower_bound = task
        .start_time
        .induced_lower_bound(context)
        .try_into()
        .unwrap();

    height + task.resource_usage > capacity
        && !(upper_bound < (lower_bound + task.processing_time)
            && has_overlap_with_interval(
                upper_bound,
                lower_bound + task.processing_time,
                start,
                end,
            ))
        && has_overlap_with_interval(lower_bound, upper_bound + task.processing_time, start, end)
        && (upper_bound + task.processing_time) > end
        && upper_bound <= end
}

impl<Var, Atomic> InferenceChecker<Atomic> for TimeTableChecker<Var>
where
    Var: CheckerVariable<Atomic>,
    Atomic: AtomicConstraint,
{
    fn check(
        &self,
        state: VariableState<Atomic>,
        _: &[Atomic],
        consequent: Option<&Atomic>,
    ) -> bool {
        // The profile is a key-value store. The keys correspond to time-points, and the values to
        // the relative change in resource consumption. A BTreeMap is used to maintain a
        // sorted order of the time points.
        let mut profile = BTreeMap::new();

        for task in self.tasks.iter() {
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

            if lst < est + task.processing_time {
                *profile.entry(lst).or_insert(0) += task.resource_usage;
                *profile.entry(est + task.processing_time).or_insert(0) -= task.resource_usage;
            }
        }

        let mut profiles = Vec::new();
        let mut current_usage = 0;
        let mut previous_time_point = *profile
            .first_key_value()
            .expect("Expected at least one mandatory part")
            .0;
        for (time_point, usage) in profile.iter() {
            if current_usage > 0 && *time_point != previous_time_point {
                profiles.push((previous_time_point, *time_point - 1, current_usage))
            }

            current_usage += *usage;

            if current_usage > self.capacity {
                return true;
            }

            previous_time_point = *time_point;
        }

        if let Some(propagating_task) = consequent.map(|consequent| {
            self.tasks
                .iter()
                .find(|task| task.start_time.does_atomic_constrain_self(consequent))
                .expect("If there is a consequent, then there should be a propagating task")
        }) {
            let mut lower_bound: i32 = propagating_task
                .start_time
                .induced_lower_bound(&state)
                .try_into()
                .unwrap();
            for (start, end_inclusive, height) in profiles.iter() {
                if lower_bound_can_be_propagated_by_profile(
                    &state,
                    lower_bound,
                    propagating_task,
                    *start,
                    *end_inclusive,
                    *height,
                    self.capacity,
                ) {
                    lower_bound = end_inclusive + 1;
                }
            }
            if lower_bound > propagating_task.start_time.induced_upper_bound(&state) {
                return true;
            }

            let mut upper_bound: i32 = propagating_task
                .start_time
                .induced_upper_bound(&state)
                .try_into()
                .unwrap();
            for (start, end_inclusive, height) in profiles.iter().rev() {
                if upper_bound_can_be_propagated_by_profile(
                    &state,
                    upper_bound,
                    propagating_task,
                    *start,
                    *end_inclusive,
                    *height,
                    self.capacity,
                ) {
                    upper_bound = start - propagating_task.processing_time;
                }
            }
            if upper_bound < propagating_task.start_time.induced_lower_bound(&state) {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_checking::TestAtomic;
    use pumpkin_checking::VariableState;

    use super::*;

    #[test]
    fn conflict() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 1,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 1,
            },
        ];

        let state = VariableState::prepare_for_conflict_check(premises, None)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 1,
                    processing_time: 1,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 1,
                    processing_time: 1,
                },
            ]
            .into(),
            capacity: 1,
        };

        assert!(checker.check(state, &premises, None));
    }

    #[test]
    fn hole_in_domain() {
        let premises = [TestAtomic {
            name: "x1",
            comparison: pumpkin_checking::Comparison::Equal,
            value: 6,
        }];

        let consequent = Some(TestAtomic {
            name: "x2",
            comparison: pumpkin_checking::Comparison::NotEqual,
            value: 2,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 3,
                    processing_time: 2,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 2,
                    processing_time: 5,
                },
            ]
            .into(),
            capacity: 4,
        };

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn lower_bound_chain() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 1,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 6,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x3",
            comparison: pumpkin_checking::Comparison::GreaterEqual,
            value: 16,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 3,
                    processing_time: 2,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 3,
                    processing_time: 10,
                },
                CheckerTask {
                    start_time: "x3",
                    resource_usage: 2,
                    processing_time: 5,
                },
            ]
            .into(),
            capacity: 4,
        };

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn upper_bound_chain() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 1,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 6,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 15,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x3",
            comparison: pumpkin_checking::Comparison::LessEqual,
            value: -4,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 3,
                    processing_time: 2,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 3,
                    processing_time: 10,
                },
                CheckerTask {
                    start_time: "x3",
                    resource_usage: 2,
                    processing_time: 5,
                },
            ]
            .into(),
            capacity: 4,
        };

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn hole_in_domain_not_accepted() {
        let premises = [TestAtomic {
            name: "x1",
            comparison: pumpkin_checking::Comparison::Equal,
            value: 6,
        }];

        let consequent = Some(TestAtomic {
            name: "x2",
            comparison: pumpkin_checking::Comparison::NotEqual,
            value: 1,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 3,
                    processing_time: 2,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 2,
                    processing_time: 5,
                },
            ]
            .into(),
            capacity: 4,
        };

        assert!(!checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn lower_bound_chain_not_accepted() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 1,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 8,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 0,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x3",
            comparison: pumpkin_checking::Comparison::GreaterEqual,
            value: 16,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 3,
                    processing_time: 2,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 3,
                    processing_time: 10,
                },
                CheckerTask {
                    start_time: "x3",
                    resource_usage: 2,
                    processing_time: 5,
                },
            ]
            .into(),
            capacity: 4,
        };

        assert!(!checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn upper_bound_chain_not_accepted() {
        let premises = [
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 1,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::Equal,
                value: 8,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 15,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x3",
            comparison: pumpkin_checking::Comparison::LessEqual,
            value: -4,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 3,
                    processing_time: 2,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 3,
                    processing_time: 10,
                },
                CheckerTask {
                    start_time: "x3",
                    resource_usage: 2,
                    processing_time: 5,
                },
            ]
            .into(),
            capacity: 4,
        };

        assert!(!checker.check(state, &premises, consequent.as_ref()));
    }

    #[test]
    fn simple_test() {
        let premises = [
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 5,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 6,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 7,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x2",
            comparison: pumpkin_checking::Comparison::LessEqual,
            value: 4,
        });
        let state = VariableState::prepare_for_conflict_check(premises, consequent)
            .expect("no conflicting atomics");

        let checker = TimeTableChecker {
            tasks: vec![
                CheckerTask {
                    start_time: "x3",
                    resource_usage: 1,
                    processing_time: 3,
                },
                CheckerTask {
                    start_time: "x2",
                    resource_usage: 2,
                    processing_time: 2,
                },
            ]
            .into(),
            capacity: 2,
        };

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }
}
