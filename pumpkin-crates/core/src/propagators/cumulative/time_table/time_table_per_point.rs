//! [`Propagator`] for the Cumulative constraint; it
//! reasons over individual time-points instead of intervals. See [`TimeTablePerPointPropagator`]
//! for more information.

use std::collections::BTreeMap;
use std::rc::Rc;

use super::time_table_util::propagate_based_on_timetable;
use super::time_table_util::should_enqueue;
use super::TimeTable;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::conjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagators::cumulative::time_table::propagation_handler::create_explanation_profile_height;
use crate::propagators::util::create_tasks;
use crate::propagators::util::register_tasks;
use crate::propagators::util::update_bounds_task;
use crate::propagators::ArgTask;
use crate::propagators::CumulativeParameters;
use crate::propagators::CumulativePropagatorOptions;
use crate::propagators::ResourceProfile;
use crate::propagators::UpdatableStructures;
use crate::pumpkin_assert_extreme;

/// [`Propagator`] responsible for using time-table reasoning to propagate the [Cumulative](https://sofdem.github.io/gccat/gccat/Ccumulative.html) constraint
/// where a time-table is a structure which stores the mandatory resource usage of the tasks at
/// different time-points - This method creates a resource profile per time point rather than
/// creating one over an interval (hence the name). Furthermore, the [`TimeTablePerPointPropagator`]
/// has a generic argument which represents the type of variable used for modelling the start
/// variables, this will be an implementation of [`IntegerVariable`].
///
/// See [Sections 4.2.1, 4.5.2 and 4.6.1-4.6.3 of \[1\]](http://cp2013.a4cp.org/sites/default/files/andreas_schutt_-_improving_scheduling_by_learning.pdf)
///  for more information about time-table reasoning.
///
/// \[1\] A. Schutt, Improving scheduling by learning. University of Melbourne, Department of
/// Computer Science and Software Engineering, 2011.
#[derive(Debug)]
pub(crate) struct TimeTablePerPointPropagator<Var, PVar, RVar, CVar> {
    /// Stores whether the time-table is empty
    is_time_table_empty: bool,
    /// Stores the input parameters to the cumulative constraint
    parameters: CumulativeParameters<Var, PVar, RVar, CVar>,
    /// Stores structures which change during the search; used to store the bounds
    updatable_structures: UpdatableStructures<Var, PVar, RVar>,

    inference_code: InferenceCode,
}

/// The type of the time-table used by propagators which use time-table reasoning per time-point;
/// using a [`ResourceProfile`] is more complex than necessary (as [`ResourceProfile::start`] =
/// [`ResourceProfile::end`]) but it allows for a more unified implementation of methods.
///
/// The key t (representing a time-point) holds the mandatory resource consumption of tasks at
/// that time (stored in a [`ResourceProfile`]); the [ResourceProfile]s are sorted based on
/// start time and they are non-overlapping
pub(crate) type PerPointTimeTableType<Var, PVar, RVar> =
    BTreeMap<u32, ResourceProfile<Var, PVar, RVar>>;

pub(crate) struct TimeTablePerPointConstructor<Var, PVar, RVar, CVar> {
    tasks: Vec<ArgTask<Var, PVar, RVar>>,
    capacity: CVar,
    cumulative_options: CumulativePropagatorOptions,
    constraint_tag: ConstraintTag,
}

impl<Var, PVar, RVar, CVar> TimeTablePerPointConstructor<Var, PVar, RVar, CVar> {
    pub(crate) fn new(
        arg_tasks: Vec<ArgTask<Var, PVar, RVar>>,
        capacity: CVar,
        cumulative_options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self {
            tasks: arg_tasks,
            capacity,
            cumulative_options,
            constraint_tag,
        }
    }
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
        CVar: IntegerVariable + 'static,
    > PropagatorConstructor for TimeTablePerPointConstructor<Var, PVar, RVar, CVar>
{
    type PropagatorImpl = TimeTablePerPointPropagator<Var, PVar, RVar, CVar>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let inference_code = context.create_inference_code(self.constraint_tag, TimeTable);

        TimeTablePerPointPropagator::new(
            context,
            &self.tasks,
            self.capacity,
            self.cumulative_options,
            inference_code,
        )
    }
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
        CVar: IntegerVariable + 'static,
    > TimeTablePerPointPropagator<Var, PVar, RVar, CVar>
{
    fn new(
        mut context: PropagatorConstructorContext,
        arg_tasks: &[ArgTask<Var, PVar, RVar>],
        capacity: CVar,
        cumulative_options: CumulativePropagatorOptions,
        inference_code: InferenceCode,
    ) -> TimeTablePerPointPropagator<Var, PVar, RVar, CVar> {
        let tasks = create_tasks(context.as_readonly(), arg_tasks);

        let parameters =
            CumulativeParameters::new(context.as_readonly(), tasks, capacity, cumulative_options);
        register_tasks(
            &parameters.tasks,
            &parameters.capacity,
            context.reborrow(),
            false,
        );

        let mut updatable_structures = UpdatableStructures::new(&parameters);
        updatable_structures.initialise_bounds_and_remove_fixed(context.as_readonly(), &parameters);

        TimeTablePerPointPropagator {
            is_time_table_empty: true,
            parameters,
            updatable_structures,
            inference_code,
        }
    }
}

impl<
        Var: IntegerVariable + 'static,
        PVar: IntegerVariable + 'static,
        RVar: IntegerVariable + 'static,
        CVar: IntegerVariable + 'static,
    > Propagator for TimeTablePerPointPropagator<Var, PVar, RVar, CVar>
{
    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        if self.parameters.is_infeasible {
            return Err(Inconsistency::Conflict(PropagatorConflict {
                conjunction: conjunction!(),
                inference_code: self.inference_code,
            }));
        }

        let time_table = create_time_table_per_point_from_scratch(
            &mut context,
            self.inference_code,
            &self.parameters,
        )?;
        self.is_time_table_empty = time_table.is_empty();
        // No error has been found -> Check for updates (i.e. go over all profiles and all tasks and
        // check whether an update can take place)
        propagate_based_on_timetable(
            &mut context,
            self.inference_code,
            time_table.values(),
            &self.parameters,
            &mut self.updatable_structures,
        )
    }

    fn synchronise(&mut self, context: PropagationContext) {
        self.updatable_structures
            .reset_all_bounds_and_remove_fixed(context, &self.parameters)
    }

    fn notify(
        &mut self,
        context: PropagationContextWithTrailedValues,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        if local_id.unpack() as usize >= self.parameters.tasks.len() {
            // The upper-bound of the capacity has been updated; we should enqueue
            return EnqueueDecision::Enqueue;
        }

        let updated_task = Rc::clone(&self.parameters.tasks[local_id.unpack() as usize]);
        // Note that it could be the case that `is_time_table_empty` is inaccurate here since it
        // wasn't updated in `synchronise`; however, `synchronise` will only remove profiles
        // meaning that `is_time_table_empty` will always return `false` when it is not
        // empty and it might return `false` even when the time-table is not empty *but* it
        // will never return `true` when the time-table is not empty.
        let result = should_enqueue(
            &self.parameters,
            &self.updatable_structures,
            &updated_task,
            context.as_readonly(),
            self.is_time_table_empty,
        );

        // Note that the non-incremental propagator does not make use of `result.updated` since it
        // propagates from scratch anyways
        update_bounds_task(
            context.as_readonly(),
            self.updatable_structures.get_stored_bounds_mut(),
            &updated_task,
        );

        if matches!(
            updated_task.start_variable.unpack_event(event),
            DomainEvent::Assign
        ) {
            self.updatable_structures.fix_task(&updated_task)
        }

        result.decision
    }

    fn priority(&self) -> u32 {
        3
    }

    fn name(&self) -> &str {
        "CumulativeTimeTablePerPoint"
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        debug_propagate_from_scratch_time_table_point(
            &mut context,
            self.inference_code,
            &self.parameters,
            &self.updatable_structures,
        )
    }
}

/// Creates a time-table consisting of [`ResourceProfile`]s which represent rectangles with a
/// start and end (both inclusive) consisting of tasks with a cumulative height Assumptions:
/// The time-table is sorted based on start time and none of the profiles overlap - generally,
/// it is assumed that the calculated [`ResourceProfile`]s are maximal
///
/// The result of this method is either the time-table of type
/// [`PerPointTimeTableType`] or the tasks responsible for the
/// conflict in the form of an [`Inconsistency`].
pub(crate) fn create_time_table_per_point_from_scratch<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    context: &mut PropagationContextMut,
    inference_code: InferenceCode,
    parameters: &CumulativeParameters<Var, PVar, RVar, CVar>,
) -> Result<PerPointTimeTableType<Var, PVar, RVar>, Inconsistency> {
    let mut time_table: PerPointTimeTableType<Var, PVar, RVar> = PerPointTimeTableType::new();
    // First we go over all tasks and determine their mandatory parts
    for task in parameters.tasks.iter() {
        let upper_bound = context.upper_bound(&task.start_variable);
        let lower_bound = context.lower_bound(&task.start_variable);

        if upper_bound < lower_bound + context.lower_bound(&task.processing_time) {
            // There is a mandatory part
            for i in upper_bound..(lower_bound + context.lower_bound(&task.processing_time)) {
                // For every time-point of the mandatory part,
                //  add the resource usage of the current task to the ResourceProfile and add it
                // to the profile tasks of the resource
                let current_profile: &mut ResourceProfile<Var, PVar, RVar> = time_table
                    .entry(i as u32)
                    .or_insert(ResourceProfile::default(i));
                current_profile.height += context.lower_bound(&task.resource_usage);
                current_profile.profile_tasks.push(Rc::clone(task));

                if current_profile.height > context.lower_bound(&parameters.capacity) {
                    context.post(
                        predicate!(parameters.capacity >= current_profile.height),
                        create_explanation_profile_height(
                            context.as_readonly(),
                            inference_code,
                            current_profile,
                            parameters.options.explanation_type,
                            parameters.capacity.clone(),
                        )
                        .conjunction,
                        inference_code,
                    )?;
                }
            }
        }
    }
    pumpkin_assert_extreme!(
        time_table
            .values()
            .all(|profile| profile.start == profile.end),
        "The TimeTablePerPointPropagator method should only create profiles where `start == end`"
    );
    Ok(time_table)
}

pub(crate) fn debug_propagate_from_scratch_time_table_point<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    context: &mut PropagationContextMut,
    inference_code: InferenceCode,
    parameters: &CumulativeParameters<Var, PVar, RVar, CVar>,
    updatable_structures: &UpdatableStructures<Var, PVar, RVar>,
) -> PropagationStatusCP {
    // We first create a time-table per point and return an error if there was
    // an overflow of the resource capacity while building the time-table
    let time_table = create_time_table_per_point_from_scratch(context, inference_code, parameters)?;
    // Then we check whether propagation can take place
    propagate_based_on_timetable(
        context,
        inference_code,
        time_table.values(),
        parameters,
        &mut updatable_structures.recreate_from_context(context.as_readonly(), parameters),
    )
}

#[cfg(test)]
mod tests {
    use crate::conjunction;
    use crate::constraint_arguments::CumulativeExplanationType;
    use crate::engine::predicates::predicate::Predicate;
    use crate::engine::propagation::EnqueueDecision;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::propagators::ArgTask;
    use crate::propagators::CumulativePropagatorOptions;
    use crate::propagators::TimeTablePerPointConstructor;

    #[test]
    fn propagator_propagates_from_profile() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
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
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(TimeTablePerPointConstructor::new(
            [
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
            .collect::<Vec<_>>(),
            1,
            CumulativePropagatorOptions {
                explanation_type: CumulativeExplanationType::Naive,
                ..Default::default()
            },
            constraint_tag,
        ));

        assert!(result.is_err());
        let reason = solver.get_reason_int(Predicate::trivially_false());
        let expected = [
            predicate!(s1 <= 1),
            predicate!(s1 >= 1),
            predicate!(s2 >= 1),
            predicate!(s2 <= 1),
        ];
        assert!(
            expected
                .iter()
                .all(|y| { reason.iter().collect::<Vec<&Predicate>>().contains(&y) })
                && reason.iter().all(|y| expected.contains(y))
        );
    }

    #[test]
    fn propagator_propagates_nothing() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(0, 6);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
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
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_after_assignment() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(0, 6);
        let s2 = solver.new_variable(6, 10);
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 6);
        assert_eq!(solver.upper_bound(s2), 10);
        assert_eq!(solver.lower_bound(s1), 0);
        assert_eq!(solver.upper_bound(s1), 6);
        let notification_status = solver.increase_lower_bound_and_notify(propagator, 0, s1, 5);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });

        let result = solver.propagate(propagator);
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
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        let result = solver.propagate_until_fixed_point(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(s2), 1);
        assert_eq!(solver.upper_bound(s2), 3);
        assert_eq!(solver.lower_bound(s1), 6);
        assert_eq!(solver.upper_bound(s1), 6);

        let reason = solver.get_reason_int(predicate!(s2 <= 3));
        assert_eq!(conjunction!([s2 <= 5] & [s1 >= 6] & [s1 <= 6]), reason);
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
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
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

        let notification_status = solver.increase_lower_bound_and_notify(propagator, 3, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(propagator);
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
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                5,
                CumulativePropagatorOptions::default(),
                constraint_tag,
            ))
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

        let notification_status = solver.increase_lower_bound_and_notify(propagator, 4, e, 3);
        assert!(match notification_status {
            EnqueueDecision::Enqueue => true,
            EnqueueDecision::Skip => false,
        });
        let result = solver.propagate(propagator);
        assert!(result.is_ok());
        assert_eq!(solver.lower_bound(f), 10);
    }

    #[test]
    fn propagator_propagates_from_profile_reason() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(1, 1);
        let s2 = solver.new_variable(1, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 1);
        assert_eq!(solver.upper_bound(s1), 1);

        let reason = solver.get_reason_int(predicate!(s2 >= 5));
        assert_eq!(
            conjunction!([s2 >= 4] & [s1 >= 1] & [s1 <= 1]), /* Note that this not
                                                              * the most general
                                                              * explanation, if s2
                                                              * could have started at
                                                              * 0 then it would still
                                                              * have
                                                              * overlapped with the
                                                              * current interval */
            reason
        );
    }

    #[test]
    fn propagator_propagates_generic_bounds() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(3, 3);
        let s2 = solver.new_variable(5, 5);
        let s3 = solver.new_variable(1, 15);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s3), 7);
        assert_eq!(solver.upper_bound(s3), 15);
        assert_eq!(solver.lower_bound(s2), 5);
        assert_eq!(solver.upper_bound(s2), 5);
        assert_eq!(solver.lower_bound(s1), 3);
        assert_eq!(solver.upper_bound(s1), 3);

        let reason = solver.get_reason_int(predicate!(s3 >= 7));
        assert_eq!(
            conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 6]), /* Note that s3 would
                                                              * have been able to
                                                              * propagate
                                                              * this bound even if it
                                                              * started at time 0 */
            reason
        );
    }

    #[test]
    fn propagator_propagates_with_holes() {
        let mut solver = TestSolver::default();
        let s1 = solver.new_variable(4, 4);
        let s2 = solver.new_variable(0, 8);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(TimeTablePerPointConstructor::new(
                [
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
                .collect::<Vec<_>>(),
                1,
                CumulativePropagatorOptions {
                    explanation_type: CumulativeExplanationType::Naive,
                    allow_holes_in_domain: true,
                    ..Default::default()
                },
                constraint_tag,
            ))
            .expect("No conflict");
        assert_eq!(solver.lower_bound(s2), 0);
        assert_eq!(solver.upper_bound(s2), 8);
        assert_eq!(solver.lower_bound(s1), 4);
        assert_eq!(solver.upper_bound(s1), 4);

        for removed in 2..8 {
            assert!(!solver.contains(s2, removed));
            let reason = solver.get_reason_int(predicate!(s2 != removed));
            assert_eq!(conjunction!([s1 <= 4] & [s1 >= 4]), reason);
        }
    }
}
