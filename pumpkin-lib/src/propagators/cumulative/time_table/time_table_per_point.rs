use std::collections::{BTreeMap, HashMap};

use crate::{
    basic_types::{
        variables::IntVar, Inconsistency, PredicateConstructor, PropagationStatusCP,
        PropositionalConjunction,
    },
    engine::{DomainChange, EnqueueDecision, PropagationContext, PropagatorVariable},
    propagators::{IncrementalPropagator, Task, Updated},
};

use super::{ResourceProfile, TimeTablePropagator};

//-------------TIME TABLE PER POINT-------------
pub struct TimeTablePerPoint {
    time_table: BTreeMap<u32, ResourceProfile>, //structure for holding time-tables
    temporary_time_table: Vec<ResourceProfile>, //TODO: there has to be a better way
    reasons_for_propagation_lower_bound: Vec<HashMap<i32, PropositionalConjunction>>, // for each variable, eagerly maps the bound change to an explanation
    reasons_for_propagation_upper_bound: Vec<HashMap<i32, PropositionalConjunction>>, // for each variable, eagerly maps the bound change to an explanation
}

impl TimeTablePerPoint {
    pub fn new<Var: IntVar + 'static>(num_tasks: usize) -> TimeTablePerPoint {
        let mut reasons_for_propagation: Vec<HashMap<i32, PropositionalConjunction>> = Vec::new();
        for _ in 0..num_tasks {
            reasons_for_propagation.push(HashMap::new());
        }
        TimeTablePerPoint {
            time_table: BTreeMap::new(),
            temporary_time_table: Vec::new(),
            reasons_for_propagation_lower_bound: reasons_for_propagation.to_vec(),
            reasons_for_propagation_upper_bound: reasons_for_propagation,
        }
    }
}

impl<Var: IntVar + 'static> TimeTablePropagator<Var> for TimeTablePerPoint {
    fn create_time_table(
        &mut self,
        context: &PropagationContext,
        tasks: &[Task<Var>],
        capacity: i32,
        reversed: bool,
    ) -> (bool, Vec<usize>) {
        let mut profile: BTreeMap<u32, ResourceProfile> = BTreeMap::new();
        for Task {
            start_variable: s,
            processing_time: p,
            resource_usage: c,
            id,
        } in tasks.iter()
        {
            let mut upper_bound = context.upper_bound(s);
            let mut lower_bound = context.lower_bound(s);
            if reversed {
                //This is to take into account scaled views, this is necessary for the following reason:
                //Let's say we have the following two tasks:
                // x: start time in [4, 7] with a processing time of 5 -> mandatory part [7, 9)
                // y: start time in [3, 3] with a processing time of 2 -> mandatory part [3, 5)
                // Which leads to no overlap
                //If we then take the scaled view we get mandatory parts from [-4, -2) and [-3, -1) which means that there is suddenly an overlap
                //So we need to take into account when variables have negative bounds!
                std::mem::swap(&mut upper_bound, &mut lower_bound);
                upper_bound = upper_bound.abs();
                lower_bound = lower_bound.abs();
                assert!(lower_bound <= upper_bound);
            }

            if upper_bound < lower_bound + p {
                for i in upper_bound..(lower_bound + p) {
                    let current_profile: &mut ResourceProfile = profile
                        .entry(i as u32)
                        .or_insert(ResourceProfile::default(i));
                    current_profile.height += c;
                    current_profile.profile_tasks.push(id.get_value());

                    if current_profile.height > capacity {
                        return (true, current_profile.profile_tasks.clone());
                    }
                }
            }
        }
        self.temporary_time_table = profile.values().cloned().collect();
        self.time_table = profile;
        (false, Vec::new())
    }

    fn store_explanation(
        &mut self,
        var: &PropagatorVariable<Var>,
        value: i32,
        explanation: PropositionalConjunction,
        lower_bound: bool,
    ) {
        if lower_bound {
            self.reasons_for_propagation_lower_bound[var.get_local_id().get_value()]
                .insert(value, explanation);
        } else {
            self.reasons_for_propagation_upper_bound[var.get_local_id().get_value()]
                .insert(value, explanation);
        }
    }

    fn get_time_table(&self) -> &Vec<ResourceProfile> {
        &self.temporary_time_table
    }
}

impl<Var: IntVar + 'static> IncrementalPropagator<Var> for TimeTablePerPoint {
    fn propagate_incrementally(
        &mut self,
        _context: &mut PropagationContext,
        _updated: &mut Vec<Updated>,
        _tasks: &[Task<Var>],
        _bounds: &mut Vec<(i32, i32)>,
        _capacity: i32,
    ) -> (
        PropagationStatusCP,
        Vec<(bool, usize, i32, PropositionalConjunction)>,
    ) {
        todo!()
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
        horizon: i32,
        capacity: i32,
        tasks_arg: &[Task<Var>],
        bounds: &[(i32, i32)],
    ) -> PropagationStatusCP {
        let mut profile: Vec<ResourceProfile> = Vec::with_capacity(horizon as usize);
        for i in 0..=horizon {
            profile.push(ResourceProfile {
                start: i,
                end: i,
                profile_tasks: Vec::new(),
                height: 0,
            });
        }
        let mut conflict = false;
        let mut conflict_profile = Vec::new();
        for Task {
            start_variable: s,
            processing_time: p,
            resource_usage: c,
            id,
        } in tasks_arg.iter()
        {
            let upper_bound = context.upper_bound(s);
            let lower_bound = context.lower_bound(s);

            if upper_bound < lower_bound + p {
                for i in upper_bound..(lower_bound + p) {
                    profile[i as usize].height += c;
                    profile[i as usize].profile_tasks.push(id.get_value());

                    if profile[i as usize].height > capacity {
                        conflict = true;
                        conflict_profile = profile[i as usize].profile_tasks.clone();
                        break;
                    }
                }
            }
        }
        {
            if conflict {
                let mut error_clause = Vec::with_capacity(conflict_profile.len() * 2);
                for task_id in conflict_profile.iter() {
                    let Task {
                        start_variable: s,
                        processing_time: _,
                        resource_usage: _,
                        id: _,
                    } = &tasks_arg[*task_id];
                    error_clause.push(s.upper_bound_predicate(context.upper_bound(s)));
                    error_clause.push(s.lower_bound_predicate(context.lower_bound(s)));
                }
                return Err(Inconsistency::from(PropositionalConjunction::from(
                    error_clause,
                )));
            }
        }
        for ResourceProfile {
            start,
            end,
            profile_tasks,
            height,
        } in profile.iter()
        {
            //go over every profile
            for Task {
                start_variable: s,
                processing_time: p,
                resource_usage: c,
                id,
            } in tasks_arg.iter()
            {
                // go over every task
                if height + c <= capacity {
                    // The tasks are sorted by capacity, if this task doesn't overload then none will
                    break;
                } else if self.has_mandatory_part_in_interval(
                    id.get_value(),
                    *start,
                    *end,
                    tasks_arg,
                    bounds,
                ) {
                    continue;
                } else if self.var_has_overlap_with_interval(context, *p, s, *start, *end) {
                    //check whether an overflow occurs + whether we can update the lower-bound
                    if (start - p) < context.lower_bound(s) && *end + 1 > context.lower_bound(s) {
                        let (conflict_found, _explanation) = self.propagate_and_explain(
                            context,
                            DomainChange::LowerBound(context.lower_bound(s)),
                            (s, *end + 1),
                            tasks_arg,
                            profile_tasks,
                        );
                        if conflict_found {
                            return Err(Inconsistency::EmptyDomain);
                        }
                    }
                    if end > &context.upper_bound(s) && *start - p < context.upper_bound(s) {
                        let (conflict_found, _explanation) = self.propagate_and_explain(
                            context,
                            DomainChange::UpperBound(context.upper_bound(s)),
                            (s, *start - p),
                            tasks_arg,
                            profile_tasks,
                        );
                        if conflict_found {
                            return Err(Inconsistency::EmptyDomain);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn propagate_from_scratch(
        &mut self,
        context: &mut PropagationContext,
        tasks: &[Task<Var>],
        bounds: &mut Vec<(i32, i32)>,
        horizon: i32,
        capacity: i32,
    ) -> (
        PropagationStatusCP,
        Vec<(bool, usize, i32, PropositionalConjunction)>,
    ) {
        TimeTablePropagator::propagate_from_scratch(self, context, tasks, bounds, horizon, capacity)
    }

    fn reset_structures(
        &mut self,
        context: &PropagationContext,
        tasks: &[Task<Var>],
        horizon: i32,
        capacity: i32,
    ) {
        TimeTablePropagator::reset_structures(self, context, tasks, horizon, capacity);
    }

    fn store_explanation(
        &mut self,
        var: &PropagatorVariable<Var>,
        value: i32,
        explanation: PropositionalConjunction,
        lower_bound: bool,
    ) {
        if lower_bound {
            self.reasons_for_propagation_lower_bound[var.get_local_id().get_value()]
                .insert(value, explanation);
        } else {
            self.reasons_for_propagation_upper_bound[var.get_local_id().get_value()]
                .insert(value, explanation);
        }
    }

    fn get_reason(
        &self,
        affected_tasks: &Task<Var>,
        change: DomainChange,
    ) -> PropositionalConjunction {
        match change {
            DomainChange::LowerBound(value) => self.reasons_for_propagation_lower_bound
                [affected_tasks.id.get_value()]
            .get(&value)
            .unwrap()
            .clone(),
            DomainChange::UpperBound(value) => self.reasons_for_propagation_upper_bound
                [affected_tasks.id.get_value()]
            .get(&value)
            .unwrap()
            .clone(),
            _ => unreachable!(),
        }
    }

    fn should_propagate(
        &mut self,
        context: &PropagationContext,
        _tasks: &[Task<Var>],
        task: &Task<Var>,
        bounds: &[(i32, i32)],
        _capacity: i32,
        updated: &mut Vec<Updated>,
    ) -> EnqueueDecision {
        TimeTablePropagator::should_propagate(self, task, context, bounds, updated)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        basic_types::{Inconsistency, Predicate, PredicateConstructor, PropositionalConjunction},
        engine::{test_helper::TestSolver, Delta, DomainChange, EnqueueDecision, LocalId},
        propagators::{ArgTask, Cumulative, CumulativeArgs, Incrementality, PropagationMethod},
    };

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let result = solver.initialise_at_root(&mut propagator);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);
        assert!(result.is_ok());
    }

    #[test]
    fn propagator_detects_conflict() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 1);

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 10,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let result = solver.initialise_at_root(&mut propagator);
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

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let result = solver.initialise_at_root(&mut propagator);
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 6);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        assert!(result.is_ok());
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

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let result = solver.initialise_at_root(&mut propagator);
        assert_eq!(solver.lower_bound(f), 10);
        assert!(result.is_ok());
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let mut result = solver.initialise_at_root(&mut propagator);
        assert_eq!(solver.lower_bound(s2), 6);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        assert!(result.is_ok());
        let notification_status = solver.increase_lower_bound(&mut propagator, 0, s1, 5);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });

        result = solver.propagate(&mut propagator);
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

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let result = solver.initialise_at_root(&mut propagator);
        assert!(result.is_ok());
        let result = solver.initialise_at_root(&mut propagator);

        assert_eq!(solver.lower_bound(s2), 1);
        //The propagator can only lower the upper-bound if there is a mandatory part which overlaps with'
        //its latest starting time, if we do not propagate until fixpoint then it is unsure whether the upper-bound has been propagated to its lowest values
        assert!(solver.upper_bound(s2) <= 4);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);
        assert!(result.is_ok());
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

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let mut result = solver.initialise_at_root(&mut propagator);
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
        assert!(result.is_ok());

        let notification_status = solver.increase_lower_bound(&mut propagator, 3, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        result = solver.propagate(&mut propagator);
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

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let mut result = solver.initialise_at_root(&mut propagator);
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
        assert!(result.is_ok());

        let notification_status = solver.increase_lower_bound(&mut propagator, 4, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        result = solver.propagate(&mut propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_from_profile_reason() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);

        let mut propagator = solver.new_propagator::<Cumulative<_>>(CumulativeArgs {
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
            horizon: 20,
            incrementality: Incrementality::REGULAR,
            propagation_method: PropagationMethod::TimeTablePerPoint,
        });
        let result = solver.initialise_at_root(&mut propagator);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);
        assert!(result.is_ok());

        let reason = solver.get_reason(
            &mut propagator,
            Delta::new(LocalId::from(1), DomainChange::LowerBound(5)),
        );
        assert_eq!(
            PropositionalConjunction::from(vec![
                s2.lower_bound_predicate(1),
                s1.lower_bound_predicate(1),
                s1.upper_bound_predicate(1),
            ]),
            reason
        );
    }
}
