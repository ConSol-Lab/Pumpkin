use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::{
    basic_types::{variables::IntVar, PropagationStatusCP, PropositionalConjunction},
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, EnqueueDecision,
        PropagationContext, PropagatorConstructorContext,
    },
    propagators::{
        CumulativeArgs, CumulativeParameters, CumulativePropagationResult, Task, Updated, Util,
    },
    pumpkin_assert_extreme,
};

use super::{
    check_for_updates_time_table, generate_update_range, ResourceProfile, TimeTablePerPoint,
    TimeTablePropagator,
};

/// Propagator responsible for using time-table reasoning to propagate the [Cumulative] constraint - This method creates a [ResourceProfile] per time point rather than creating one over an interval
pub struct TimeTablePerPointIncremental<Var> {
    /// * `time_table` - Structure responsible for holding the time-table; currently it consists of a map to avoid unnecessary allocation for time-points at which no [ResourceProfile] is present | Assumptions: The time-table is sorted based on start time and none of the profiles overlap - generally, it is assumed that the calculated [ResourceProfile]s are maximal
    time_table: BTreeMap<u32, ResourceProfile<Var>>,
    /// * `reasons_for_propagation_lower_bound` - For each variable, eagerly maps the explanation of the lower-bound change
    reasons_for_propagation_lower_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// * `reasons_for_propagation_upper_bound` - For each variable, eagerly maps the explanation of the upper-bound change
    reasons_for_propagation_upper_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// * `cumulative_params` - Stores the input parameters to the cumulative constraint
    cumulative_params: CumulativeParameters<Var>,
}

impl<Var> CPPropagatorConstructor for TimeTablePerPointIncremental<Var>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Args = CumulativeArgs<Var>;

    fn create(
        args: Self::Args,
        context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        Box::new(TimeTablePerPointIncremental::new(
            Util::create_tasks(&args.tasks, context),
            args.capacity,
        ))
    }
}

impl<Var: IntVar + 'static> TimeTablePerPointIncremental<Var> {
    pub fn new(tasks: Vec<Task<Var>>, capacity: i32) -> TimeTablePerPointIncremental<Var> {
        let reasons_for_propagation: Vec<HashMap<i32, PropositionalConjunction>> =
            vec![HashMap::new(); tasks.len()];
        TimeTablePerPointIncremental {
            time_table: BTreeMap::new(),
            reasons_for_propagation_lower_bound: reasons_for_propagation.to_vec(),
            reasons_for_propagation_upper_bound: reasons_for_propagation,
            cumulative_params: CumulativeParameters::create(tasks, capacity),
        }
    }
}

impl<Var: IntVar + 'static> ConstraintProgrammingPropagator for TimeTablePerPointIncremental<Var> {
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        Util::check_bounds_equal_at_propagation(
            context,
            &self.cumulative_params.tasks,
            &self.cumulative_params.bounds,
        );
        for Updated {
            task,
            old_lower_bound,
            old_upper_bound,
            new_lower_bound,
            new_upper_bound,
        } in self.cumulative_params.updated.iter()
        {
            //Go over all of the updated tasks and calculate the added mandatory part (we know that for each of these tasks, a mandatory part exists, otherwise it would not have been added (see [TimeTablePropagator::should_propagate]))
            let (lower_update_range, upper_update_range) = generate_update_range(
                task,
                *old_lower_bound,
                *old_upper_bound,
                *new_lower_bound,
                *new_upper_bound,
            );

            for t in lower_update_range
                .into_iter()
                .chain(upper_update_range.into_iter())
            {
                //We first go over the lower update range and then we go over the lower update range
                pumpkin_assert_extreme!(
                    !self.time_table.contains_key(&(t as u32))
                    || !self.time_table.get(&(t as u32)).unwrap().profile_tasks.iter().any(|profile_task| profile_task.id.get_value() == task.id.get_value()),
                    "Attempted to insert mandatory part where it already exists at time point {t} for task {} in time-table per time-point propagator", task.id.get_value());

                //Add the updated profile to the ResourceProfile at time t
                let current_profile: &mut ResourceProfile<Var> = self
                    .time_table
                    .entry(t as u32)
                    .or_insert(ResourceProfile::default(t));
                current_profile.height += task.resource_usage;
                current_profile.profile_tasks.push(Rc::clone(task));
                if current_profile.height > self.cumulative_params.capacity {
                    //The newly introduced mandatory part(s) caused an overflow of the resource
                    return Util::create_error_clause(context, &current_profile.profile_tasks);
                }
            }
        }
        self.cumulative_params.updated.clear(); //We have processed all of the updates, we can clear the structure
                                                //We pass the entirety of the table to check due to the fact that the propagation of the current profile could lead to the propagation across multiple profiles
                                                //For example, if we have updated 1 resource profile which caused a propagation then this could cause another propagation by a profile which has not been updated
        let CumulativePropagationResult {
            status,
            explanations,
        } = check_for_updates_time_table(
            context,
            self.time_table.values(),
            self.time_table.len(),
            self.get_parameters(),
        );

        Util::store_explanations(
            explanations,
            &mut self.reasons_for_propagation_lower_bound,
            &mut self.reasons_for_propagation_upper_bound,
        );
        status
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        Util::reset_bounds_clear_updated(
            context,
            &mut self.cumulative_params.updated,
            &mut self.cumulative_params.bounds,
            &self.cumulative_params.tasks,
        );
        TimeTablePropagator::reset_structures(self, context);
    }

    fn get_reason_for_propagation(
        &mut self,
        _context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        Util::get_reason_for_propagation(
            delta,
            &self.reasons_for_propagation_lower_bound,
            &self.reasons_for_propagation_upper_bound,
            &self.cumulative_params.tasks,
        )
    }

    fn notify(
        &mut self,
        context: &mut PropagationContext,
        local_id: crate::engine::LocalId,
        _event: crate::engine::OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let task = &self.cumulative_params.tasks[local_id.get_value()];
        pumpkin_assert_extreme!(
            context.lower_bound(&task.start_variable)
                > self.cumulative_params.bounds[task.id.get_value()].0
                || self.cumulative_params.bounds[task.id.get_value()].1
                    >= context.upper_bound(&task.start_variable)
        );
        let old_lower_bound = self.cumulative_params.bounds[task.id.get_value()].0;
        let old_upper_bound = self.cumulative_params.bounds[task.id.get_value()].1;
        //We check whether a mandatory part was extended/introduced
        if context.upper_bound(&task.start_variable)
            < context.lower_bound(&task.start_variable) + task.processing_time
        {
            self.cumulative_params.updated.push(Updated {
                task: Rc::clone(task),
                old_lower_bound,
                old_upper_bound,
                new_lower_bound: context.lower_bound(&task.start_variable),
                new_upper_bound: context.upper_bound(&task.start_variable),
            });
        }
        Util::update_bounds_task(context, &mut self.cumulative_params.bounds, task);
        EnqueueDecision::Enqueue // TODO: for some reason skipping here and only returning when a new mandatory part is added will result in missed propagations
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPoint"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        Util::initialise_at_root(true, &mut self.cumulative_params, context);
        let CumulativePropagationResult {
            status,
            explanations,
        } = TimeTablePropagator::propagate_from_scratch(self, context);

        Util::store_explanations(
            explanations,
            &mut self.reasons_for_propagation_lower_bound,
            &mut self.reasons_for_propagation_upper_bound,
        );
        status
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        //Use the same debug propagator from `TimeTablePerPoint`
        TimeTablePerPoint::debug_propagate_from_scratch_time_table_point(
            context,
            self.get_parameters(),
        )
    }
}

impl<Var: IntVar + 'static> TimeTablePropagator<Var> for TimeTablePerPointIncremental<Var> {
    type TimeTableIterator<'b> = std::collections::btree_map::Values<'b, u32, ResourceProfile<Var>>;

    type TimeTableType = BTreeMap<u32, ResourceProfile<Var>>;

    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContext,
    ) -> Option<Vec<Rc<Task<Var>>>> {
        match <TimeTablePerPointIncremental<Var> as TimeTablePropagator<Var>>::create_time_table(
            context,
            self.get_parameters(),
        ) {
            Ok(result) => {
                self.time_table = result;
                None
            }
            Err(explanation) => Some(explanation),
        }
    }

    fn create_time_table(
        context: &PropagationContext,
        params: &CumulativeParameters<Var>,
    ) -> Result<Self::TimeTableType, Vec<Rc<Task<Var>>>> {
        let mut profile: BTreeMap<u32, ResourceProfile<Var>> = BTreeMap::new();
        //First we go over all tasks and determine their mandatory parts
        for task in params.tasks.iter() {
            let upper_bound = context.upper_bound(&task.start_variable);
            let lower_bound = context.lower_bound(&task.start_variable);

            if upper_bound < lower_bound + task.processing_time {
                //There is a mandatory part
                for i in upper_bound..(lower_bound + task.processing_time) {
                    //For every time-point of the mandatory part, add the resource usage of the current task to the ResourceProfile and add it to the profile tasks of the resource
                    let current_profile: &mut ResourceProfile<Var> = profile
                        .entry(i as u32)
                        .or_insert(ResourceProfile::default(i));
                    current_profile.height += task.resource_usage;
                    current_profile.profile_tasks.push(Rc::clone(task));

                    if current_profile.height > params.capacity {
                        //The addition of the current task to the resource profile has caused an overflow
                        return Err(current_profile.profile_tasks.clone());
                    }
                }
            }
        }
        Ok(profile)
    }

    fn get_parameters(&self) -> &CumulativeParameters<Var> {
        &self.cumulative_params
    }

    fn get_time_table_and_length(&self) -> (Self::TimeTableIterator<'_>, usize) {
        (self.time_table.values(), self.time_table.len())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        basic_types::{Inconsistency, Predicate, PredicateConstructor, PropositionalConjunction},
        engine::{test_helper::TestSolver, Delta, DomainChange, EnqueueDecision, LocalId},
        propagators::{ArgTask, CumulativeArgs, TimeTablePerPointIncremental},
    };

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 1,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);
    }

    #[test]
    fn propagator_detects_conflict() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 1);

        let result = solver.new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
            tasks: [
                ArgTask {
                    start_time: s1,
                    processing_time: 4,
                    resource_usage: 1,
                },
                ArgTask {
                    start_time: s2,
                    processing_time: 4,
                    resource_usage: 1,
                },
            ]
            .into_iter()
            .collect(),
            capacity: 1,
        });
        assert!(match result {
            Err(inconsistency) => {
                match inconsistency {
                    Inconsistency::EmptyDomain => false,
                    Inconsistency::Other(x) => {
                        let expected = [
                            s1.upper_bound_predicate(1),
                            s1.lower_bound_predicate(1),
                            s2.upper_bound_predicate(1),
                            s2.lower_bound_predicate(1),
                        ];
                        expected
                            .iter()
                            .all(|y| x.iter().collect::<Vec<&Predicate>>().contains(&y))
                            && x.iter().all(|y| expected.contains(y))
                    }
                }
            }
            _ => false,
        });
    }

    #[test]
    fn propagator_propagates_nothing() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(0, 6);

        solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 1,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 6);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(2, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b = solver.new_variable(2, 3);
        let a = solver.new_variable(0, 1);

        solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 5,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 5,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);

        let mut propagator = solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 1,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 6);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        let notification_status = solver.increase_lower_bound(&mut propagator, 0, s1, 5);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });

        let result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s2), 7);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 5);
        assert_eq!(solver.upper_bound(s1), 6);
    }

    #[test]
    fn propagator_propagates_end_time() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(6, 6);
        let s2 = solver.new_variable(1, 8);

        let mut propagator = solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 1,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::UpperBound(3)),
        );
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.upper_bound_predicate(9),
                s1.lower_bound_predicate(6),
                s1.upper_bound_predicate(6),
            ]),
            reason
        );
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt_after_update() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(0, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b = solver.new_variable(2, 3);
        let a = solver.new_variable(0, 1);

        let mut propagator = solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 4,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 5,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(a), 0);
        assert_eq!(solver.upper_bound(a), 1);
        assert_eq!(solver.lower_bound(b), 2);
        assert_eq!(solver.upper_bound(b), 3);
        assert_eq!(solver.lower_bound(c), 8);
        assert_eq!(solver.upper_bound(c), 9);
        assert_eq!(solver.lower_bound(d), 0);
        assert_eq!(solver.upper_bound(d), 2);
        assert_eq!(solver.lower_bound(e), 0);
        assert_eq!(solver.upper_bound(e), 4);
        assert_eq!(solver.lower_bound(f), 0);
        assert_eq!(solver.upper_bound(f), 14);

        let notification_status = solver.increase_lower_bound(&mut propagator, 3, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_example_4_3_schutt_multiple_profiles() {
        let mut solver = TestSolver::default();
        let f = solver.new_variable(0, 14);
        let e = solver.new_variable(0, 4);
        let d = solver.new_variable(0, 2);
        let c = solver.new_variable(8, 9);
        let b2 = solver.new_variable(5, 5);
        let b1 = solver.new_variable(3, 3);
        let a = solver.new_variable(0, 1);

        let mut propagator = solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: a,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: b1,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: b2,
                        processing_time: 3,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: c,
                        processing_time: 2,
                        resource_usage: 4,
                    },
                    ArgTask {
                        start_time: d,
                        processing_time: 2,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: e,
                        processing_time: 4,
                        resource_usage: 2,
                    },
                    ArgTask {
                        start_time: f,
                        processing_time: 6,
                        resource_usage: 2,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 5,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(a), 0);
        assert_eq!(solver.upper_bound(a), 1);
        assert_eq!(solver.lower_bound(c), 8);
        assert_eq!(solver.upper_bound(c), 9);
        assert_eq!(solver.lower_bound(d), 0);
        assert_eq!(solver.upper_bound(d), 2);
        assert_eq!(solver.lower_bound(e), 0);
        assert_eq!(solver.upper_bound(e), 4);
        assert_eq!(solver.lower_bound(f), 0);
        assert_eq!(solver.upper_bound(f), 14);

        let notification_status = solver.increase_lower_bound(&mut propagator, 4, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_from_profile_reason() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let mut propagator = solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: s1,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 3,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 1,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::LowerBound(5)),
        );
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.lower_bound_predicate(0), //Note that this is a more general explanation, if s2 could have started at 0 then it would still have overlapped with the current interval
                s1.lower_bound_predicate(1),
                s1.upper_bound_predicate(1),
            ]),
            reason
        );
    }

    #[test]
    fn propagator_propagates_generic_bounds() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(3, 3);
        let s2 = solver.new_variable(5, 5);
        let s3 = solver.new_variable(1, 15);

        let mut propagator = solver
            .new_propagator::<TimeTablePerPointIncremental<_>>(CumulativeArgs {
                tasks: [
                    ArgTask {
                        start_time: s1,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s2,
                        processing_time: 2,
                        resource_usage: 1,
                    },
                    ArgTask {
                        start_time: s3,
                        processing_time: 4,
                        resource_usage: 1,
                    },
                ]
                .into_iter()
                .collect(),
                capacity: 1,
            })
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(2), DomainChange::LowerBound(7)),
        );
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.upper_bound_predicate(5),
                s2.lower_bound_predicate(5),
                s1.lower_bound_predicate(3),
                s1.upper_bound_predicate(3),
                s3.lower_bound_predicate(0), //Note that s3 would have been able to propagate this bound even if it started at time 0
            ]),
            reason
        );
    }
}
