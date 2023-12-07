use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, PropagationStatusCP, PropositionalConjunction,
    },
    engine::{
        CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange,
        EnqueueDecision, PropagationContext, PropagatorConstructorContext,
    },
    propagators::{CumulativeArgs, CumulativeParameters, CumulativePropagationResult, Task, Util},
};

use super::{ResourceProfile, TimeTableCreationResult, TimeTablePropagator};

/// Propagator responsible for using time-table reasoning to propagate the [Cumulative] constraint - This method creates a [ResourceProfile] per time point rather than creating one over an interval
pub struct TimeTablePerPoint<Var> {
    /// * `time_table` - Structure responsible for holding the time-table; currently it consists of a map to avoid unnecessary allocation for time-points at which no [ResourceProfile] is present | Assumptions: The time-table is sorted based on start time and none of the profiles overlap - generally, it is assumed that the calculated [ResourceProfile]s are maximal
    time_table: BTreeMap<u32, ResourceProfile<Var>>,
    /// * `reasons_for_propagation_lower_bound` - For each variable, eagerly maps the explanation of the lower-bound change
    reasons_for_propagation_lower_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// * `reasons_for_propagation_upper_bound` - For each variable, eagerly maps the explanation of the upper-bound change
    reasons_for_propagation_upper_bound: Vec<HashMap<i32, PropositionalConjunction>>,
    /// * `cumulative_params` - Stores the input parameters to the cumulative constraint
    cumulative_params: CumulativeParameters<Var>,
}

impl<Var> CPPropagatorConstructor for TimeTablePerPoint<Var>
where
    Var: IntVar + 'static + std::fmt::Debug,
{
    type Args = CumulativeArgs<Var>;

    fn create(
        args: Self::Args,
        context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        Box::new(TimeTablePerPoint::new(
            Util::create_tasks(&args.tasks, context),
            args.capacity,
        ))
    }
}

impl<Var: IntVar + 'static> TimeTablePerPoint<Var> {
    pub fn new(tasks: Vec<Task<Var>>, capacity: i32) -> TimeTablePerPoint<Var> {
        let reasons_for_propagation: Vec<HashMap<i32, PropositionalConjunction>> =
            vec![HashMap::new(); tasks.len()];
        TimeTablePerPoint {
            time_table: BTreeMap::new(),
            reasons_for_propagation_lower_bound: reasons_for_propagation.to_vec(),
            reasons_for_propagation_upper_bound: reasons_for_propagation,
            cumulative_params: CumulativeParameters::create(tasks, capacity),
        }
    }
}

impl<Var: IntVar + 'static> ConstraintProgrammingPropagator for TimeTablePerPoint<Var> {
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
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

    fn synchronise(&mut self, context: &PropagationContext) {
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
        _context: &mut PropagationContext,
        _local_id: crate::engine::LocalId,
        _event: crate::engine::OpaqueDomainEvent,
    ) -> EnqueueDecision {
        //Propagator from scratch, always enqueue
        EnqueueDecision::Enqueue
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPoint"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        Util::initialise_at_root(false, &mut self.cumulative_params, context);
        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        //This method is similar to that of `create_time_table` but somewhat simpler
        //We first create a time-table per point in the horizon
        let horizon = self
            .cumulative_params
            .tasks
            .iter()
            .map(|current| current.processing_time)
            .sum::<i32>();
        let mut profile: Vec<ResourceProfile<Var>> = Vec::with_capacity(horizon as usize);
        for i in 0..=horizon {
            profile.push(ResourceProfile {
                start: i,
                end: i,
                profile_tasks: Vec::new(),
                height: 0,
            });
        }
        for task in self.cumulative_params.tasks.iter() {
            let upper_bound = context.upper_bound(&task.start_variable);
            let lower_bound = context.lower_bound(&task.start_variable);

            if upper_bound < lower_bound + task.processing_time {
                //For each task, we check whether there now exists a mandatory part
                //If this is the case then we add the task we are currently processing (i.e. `task`) to the created or existing profile
                for i in upper_bound..(lower_bound + task.processing_time) {
                    profile[i as usize].height += task.resource_usage;
                    profile[i as usize].profile_tasks.push(Rc::clone(task));

                    if profile[i as usize].height > self.cumulative_params.capacity {
                        //The profile which we have just added to has overflown the resource capacity
                        return Util::create_error_clause(
                            context,
                            &profile[i as usize].profile_tasks,
                        );
                    }
                }
            }
        }
        //Now we need to propagate the tasks appropriately, first we go over all profiles
        for ResourceProfile {
            start,
            end,
            profile_tasks,
            height,
        } in profile.iter()
        {
            //Then we go over all tasks
            for task in self.cumulative_params.tasks.iter() {
                if height + task.resource_usage <= self.cumulative_params.capacity {
                    //The tasks are sorted in non-increasing order of resource usage, if this holds for a task then it will hold for all subsequent tasks
                    break;
                } else if self.has_mandatory_part_in_interval(context, task, *start, *end) {
                    //The task has a mandatory part here already, it cannot be propagated due the current profile
                    continue;
                } else if self.var_has_overlap_with_interval(context, task, *start, *end) {
                    if (start - task.processing_time) < context.lower_bound(&task.start_variable)
                        && *end + 1 > context.lower_bound(&task.start_variable)
                        && Util::propagate_and_explain(
                            context,
                            DomainChange::LowerBound(context.lower_bound(&task.start_variable)),
                            task,
                            *end + 1,
                            profile_tasks,
                        )
                        .is_err()
                    {
                        //We do not need to store explanations so we simply check whether the propagation of the lower bound resulted in an error
                        return Err(Inconsistency::EmptyDomain);
                    }
                    if end > &context.upper_bound(&task.start_variable)
                        && *start - task.processing_time < context.upper_bound(&task.start_variable)
                        && Util::propagate_and_explain(
                            context,
                            DomainChange::UpperBound(context.upper_bound(&task.start_variable)),
                            task,
                            *start - task.processing_time,
                            profile_tasks,
                        )
                        .is_err()
                    {
                        //We do not need to store explanations so we simply check whether the propagation of the upper bound resulted in an error
                        return Err(Inconsistency::EmptyDomain);
                    }
                }
            }
        }
        Ok(())
    }
}

impl<Var: IntVar + 'static> TimeTablePropagator<Var> for TimeTablePerPoint<Var> {
    fn create_time_table_and_assign(
        &mut self,
        context: &PropagationContext,
    ) -> TimeTableCreationResult<Var> {
        let result = self.create_time_table(context)?;
        self.time_table = result;
        Ok(self.time_table.clone())
    }

    fn create_time_table(&self, context: &PropagationContext) -> TimeTableCreationResult<Var> {
        let mut profile: BTreeMap<u32, ResourceProfile<Var>> = BTreeMap::new();
        //First we go over all tasks and determine their mandatory parts
        for task in self.cumulative_params.tasks.iter() {
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

                    if current_profile.height > self.cumulative_params.capacity {
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
}

#[cfg(test)]
mod tests {
    use crate::{
        basic_types::{Inconsistency, Predicate, PredicateConstructor, PropositionalConjunction},
        engine::{test_helper::TestSolver, Delta, DomainChange, EnqueueDecision, LocalId},
        propagators::{ArgTask, CumulativeArgs, TimeTablePerPoint},
    };

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        solver
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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

        let result = solver.new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
            .new_propagator::<TimeTablePerPoint<_>>(CumulativeArgs {
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
