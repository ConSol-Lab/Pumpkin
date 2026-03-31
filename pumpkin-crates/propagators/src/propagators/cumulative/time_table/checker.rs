use std::collections::BTreeMap;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_checking::IntExt;
use pumpkin_checking::VariableState;

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

    #[test]
    fn test_holes_in_domain() {
        let premises = [
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 1,
            },
            TestAtomic {
                name: "x3",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 3,
            },
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 4,
            },
            TestAtomic {
                name: "x1",
                comparison: pumpkin_checking::Comparison::LessEqual,
                value: 4,
            },
            TestAtomic {
                name: "x2",
                comparison: pumpkin_checking::Comparison::GreaterEqual,
                value: 2,
            },
        ];

        let consequent = Some(TestAtomic {
            name: "x2",
            comparison: pumpkin_checking::Comparison::GreaterEqual,
            value: 5,
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
                CheckerTask {
                    start_time: "x1",
                    resource_usage: 1,
                    processing_time: 1,
                },
            ]
            .into(),
            capacity: 2,
        };

        assert!(checker.check(state, &premises, consequent.as_ref()));
    }
}
