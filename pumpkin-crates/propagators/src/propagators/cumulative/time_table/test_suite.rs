//! Tests which are shared between all time-table propagators.
//!
//! Every test is written once as a function which is generic over the propagator under test; the
//! [`time_table_tests`] and [`synchronisation_tests`] macros instantiate them for each propagator.
use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::propagation::CurrentNogood;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::state::Conflict;
use pumpkin_core::state::State;
use pumpkin_core::variables::DomainId;

use crate::StateExt;
use crate::cumulative::ArgTask;
use crate::cumulative::options::CumulativePropagatorOptions;
use crate::cumulative::time_table::CumulativeExplanationType;
use crate::cumulative::time_table::TimeTableOverIntervalIncrementalPropagator;
use crate::cumulative::time_table::TimeTableOverIntervalPropagator;
use crate::cumulative::time_table::TimeTablePerPointIncrementalPropagator;
use crate::cumulative::time_table::TimeTablePerPointPropagator;

/// The way in which the time-table is constructed; the explanations of the
/// propagators reasoning over intervals can differ from those which reason per point.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TimeTableStrategy {
    PerPoint,
    OverInterval,
}

/// A time-table propagator which can be tested by the shared test suite.
trait TimeTableTestCase: PropagatorConstructor<PropagatorImpl: 'static> + Sized {
    const STRATEGY: TimeTableStrategy;

    fn construct(
        tasks: &[ArgTask<DomainId>],
        capacity: i32,
        options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self;
}

impl TimeTableTestCase for TimeTablePerPointPropagator<DomainId> {
    const STRATEGY: TimeTableStrategy = TimeTableStrategy::PerPoint;

    fn construct(
        tasks: &[ArgTask<DomainId>],
        capacity: i32,
        options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self::new(tasks, capacity, options, constraint_tag)
    }
}

impl TimeTableTestCase for TimeTableOverIntervalPropagator<DomainId> {
    const STRATEGY: TimeTableStrategy = TimeTableStrategy::OverInterval;

    fn construct(
        tasks: &[ArgTask<DomainId>],
        capacity: i32,
        options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self::new(tasks, capacity, options, constraint_tag)
    }
}

impl<const SYNCHRONISE: bool> TimeTableTestCase
    for TimeTablePerPointIncrementalPropagator<DomainId, SYNCHRONISE>
{
    const STRATEGY: TimeTableStrategy = TimeTableStrategy::PerPoint;

    fn construct(
        tasks: &[ArgTask<DomainId>],
        capacity: i32,
        options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self::new(tasks, capacity, options, constraint_tag)
    }
}

impl<const SYNCHRONISE: bool> TimeTableTestCase
    for TimeTableOverIntervalIncrementalPropagator<DomainId, SYNCHRONISE>
{
    const STRATEGY: TimeTableStrategy = TimeTableStrategy::OverInterval;

    fn construct(
        tasks: &[ArgTask<DomainId>],
        capacity: i32,
        options: CumulativePropagatorOptions,
        constraint_tag: ConstraintTag,
    ) -> Self {
        Self::new(tasks, capacity, options, constraint_tag)
    }
}

fn naive_options() -> CumulativePropagatorOptions {
    CumulativePropagatorOptions {
        explanation_type: CumulativeExplanationType::Naive,
        ..Default::default()
    }
}

/// Adds propagator `P` over the tasks, given as `(start_time, processing_time, resource_usage)`,
/// to `state` and propagates to a fixed point.
fn add_propagator<P: TimeTableTestCase>(
    state: &mut State,
    tasks: &[(DomainId, i32, i32)],
    capacity: i32,
    options: CumulativePropagatorOptions,
) -> Result<(), Conflict> {
    let tasks = tasks
        .iter()
        .map(|&(start_time, processing_time, resource_usage)| ArgTask {
            start_time,
            processing_time,
            resource_usage,
        })
        .collect::<Vec<_>>();
    let constraint_tag = state.new_constraint_tag();
    let _ = state.add_propagator(P::construct(&tasks, capacity, options, constraint_tag));
    state.propagate_to_fixed_point()
}

fn reason_for(state: &mut State, predicate: Predicate) -> PropositionalConjunction {
    let mut reason_buffer: Vec<Predicate> = vec![];
    let _ = state.get_propagation_reason(predicate, &mut reason_buffer, CurrentNogood::empty());
    reason_buffer.into()
}

fn conflict_explanation(result: Result<(), Conflict>) -> PropositionalConjunction {
    let Err(Conflict::Propagator(conflict)) = result else {
        panic!("an explicit conflict should have been detected, but was {result:?}");
    };
    conflict.conjunction
}

fn propagator_propagates_from_profile<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(1, 1, None);
    let s2 = state.new_interval_variable(1, 8, None);

    add_propagator::<P>(&mut state, &[(s1, 4, 1), (s2, 3, 1)], 1, Default::default())
        .expect("No conflict");
    state.assert_bounds(s1, 1, 1);
    state.assert_bounds(s2, 5, 8);
}

fn propagator_detects_conflict<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(1, 1, None);
    let s2 = state.new_interval_variable(1, 1, None);

    let result = add_propagator::<P>(&mut state, &[(s1, 4, 1), (s2, 4, 1)], 1, naive_options());
    assert_eq!(
        conjunction!([s1 <= 1] & [s1 >= 1] & [s2 <= 1] & [s2 >= 1]),
        conflict_explanation(result)
    );
}

fn propagator_propagates_nothing<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(0, 6, None);
    let s2 = state.new_interval_variable(0, 6, None);

    add_propagator::<P>(&mut state, &[(s1, 4, 1), (s2, 3, 1)], 1, Default::default())
        .expect("No conflict");
    state.assert_bounds(s1, 0, 6);
    state.assert_bounds(s2, 0, 6);
}

fn propagator_propagates_example_4_3_schutt<P: TimeTableTestCase>() {
    let mut state = State::default();
    let f = state.new_interval_variable(0, 14, None);
    let e = state.new_interval_variable(2, 4, None);
    let d = state.new_interval_variable(0, 2, None);
    let c = state.new_interval_variable(8, 9, None);
    let b = state.new_interval_variable(2, 3, None);
    let a = state.new_interval_variable(0, 1, None);

    add_propagator::<P>(
        &mut state,
        &[
            (a, 2, 1),
            (b, 6, 2),
            (c, 2, 4),
            (d, 2, 2),
            (e, 5, 2),
            (f, 6, 2),
        ],
        5,
        Default::default(),
    )
    .expect("No conflict");
    assert_eq!(state.lower_bound(f), 10);
}

fn propagator_propagates_after_assignment<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(0, 6, None);
    let s2 = state.new_interval_variable(6, 10, None);

    add_propagator::<P>(&mut state, &[(s1, 2, 1), (s2, 3, 1)], 1, Default::default())
        .expect("No conflict");
    state.assert_bounds(s1, 0, 6);
    state.assert_bounds(s2, 6, 10);

    let _ = state.post(predicate!(s1 >= 5)).expect("No empty domain");
    state.propagate_to_fixed_point().expect("No conflict");
    state.assert_bounds(s1, 5, 6);
    state.assert_bounds(s2, 7, 10);
}

fn propagator_propagates_end_time<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(6, 6, None);
    let s2 = state.new_interval_variable(1, 8, None);

    add_propagator::<P>(&mut state, &[(s1, 4, 1), (s2, 3, 1)], 1, naive_options())
        .expect("No conflict");
    state.assert_bounds(s1, 6, 6);
    state.assert_bounds(s2, 1, 3);

    let expected = match P::STRATEGY {
        TimeTableStrategy::PerPoint => conjunction!([s2 <= 5] & [s1 >= 6] & [s1 <= 6]),
        TimeTableStrategy::OverInterval => conjunction!([s2 <= 8] & [s1 >= 6] & [s1 <= 6]),
    };
    assert_eq!(expected, reason_for(&mut state, predicate!(s2 <= 3)));
}

fn propagator_propagates_example_4_3_schutt_after_update<P: TimeTableTestCase>() {
    let mut state = State::default();
    let f = state.new_interval_variable(0, 14, None);
    let e = state.new_interval_variable(0, 4, None);
    let d = state.new_interval_variable(0, 2, None);
    let c = state.new_interval_variable(8, 9, None);
    let b = state.new_interval_variable(2, 3, None);
    let a = state.new_interval_variable(0, 1, None);

    add_propagator::<P>(
        &mut state,
        &[
            (a, 2, 1),
            (b, 6, 2),
            (c, 2, 4),
            (d, 2, 2),
            (e, 4, 2),
            (f, 6, 2),
        ],
        5,
        Default::default(),
    )
    .expect("No conflict");
    state.assert_bounds(a, 0, 1);
    state.assert_bounds(b, 2, 3);
    state.assert_bounds(c, 8, 9);
    state.assert_bounds(d, 0, 2);
    state.assert_bounds(e, 0, 4);
    state.assert_bounds(f, 0, 14);

    let _ = state.post(predicate!(e >= 3)).expect("No empty domain");
    state.propagate_to_fixed_point().expect("No conflict");
    assert_eq!(state.lower_bound(f), 10);
}

fn propagator_propagates_example_4_3_schutt_multiple_profiles<P: TimeTableTestCase>() {
    let mut state = State::default();
    let f = state.new_interval_variable(0, 14, None);
    let e = state.new_interval_variable(0, 4, None);
    let d = state.new_interval_variable(0, 2, None);
    let c = state.new_interval_variable(8, 9, None);
    let b2 = state.new_interval_variable(5, 5, None);
    let b1 = state.new_interval_variable(3, 3, None);
    let a = state.new_interval_variable(0, 1, None);

    add_propagator::<P>(
        &mut state,
        &[
            (a, 2, 1),
            (b1, 2, 2),
            (b2, 3, 2),
            (c, 2, 4),
            (d, 2, 2),
            (e, 4, 2),
            (f, 6, 2),
        ],
        5,
        Default::default(),
    )
    .expect("No conflict");
    state.assert_bounds(a, 0, 1);
    state.assert_bounds(c, 8, 9);
    state.assert_bounds(d, 0, 2);
    state.assert_bounds(e, 0, 4);
    state.assert_bounds(f, 0, 14);

    let _ = state.post(predicate!(e >= 3)).expect("No empty domain");
    state.propagate_to_fixed_point().expect("No conflict");
    assert_eq!(state.lower_bound(f), 10);
}

fn propagator_propagates_from_profile_reason<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(1, 1, None);
    let s2 = state.new_interval_variable(1, 8, None);

    add_propagator::<P>(&mut state, &[(s1, 4, 1), (s2, 3, 1)], 1, naive_options())
        .expect("No conflict");
    state.assert_bounds(s1, 1, 1);
    state.assert_bounds(s2, 5, 8);

    let expected = match P::STRATEGY {
        // Note that this not the most general explanation, if s2 could have started at 0 then it
        // would still have overlapped with the current interval
        TimeTableStrategy::PerPoint => conjunction!([s2 >= 4] & [s1 >= 1] & [s1 <= 1]),
        TimeTableStrategy::OverInterval => conjunction!([s2 >= 1] & [s1 >= 1] & [s1 <= 1]),
    };
    assert_eq!(expected, reason_for(&mut state, predicate!(s2 >= 5)));
}

fn propagator_propagates_generic_bounds<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(3, 3, None);
    let s2 = state.new_interval_variable(5, 5, None);
    let s3 = state.new_interval_variable(1, 15, None);

    add_propagator::<P>(
        &mut state,
        &[(s1, 2, 1), (s2, 2, 1), (s3, 4, 1)],
        1,
        naive_options(),
    )
    .expect("No conflict");
    state.assert_bounds(s1, 3, 3);
    state.assert_bounds(s2, 5, 5);
    state.assert_bounds(s3, 7, 15);

    let expected = match P::STRATEGY {
        // Note that s3 would have been able to propagate this bound even if it started at time 0
        TimeTableStrategy::PerPoint => conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 6]),
        TimeTableStrategy::OverInterval => conjunction!([s2 <= 5] & [s2 >= 5] & [s3 >= 5]),
    };
    assert_eq!(expected, reason_for(&mut state, predicate!(s3 >= 7)));
}

fn propagator_propagates_with_holes<P: TimeTableTestCase>() {
    let mut state = State::default();
    let s1 = state.new_interval_variable(4, 4, None);
    let s2 = state.new_interval_variable(0, 8, None);

    let options = CumulativePropagatorOptions {
        allow_holes_in_domain: true,
        ..naive_options()
    };
    add_propagator::<P>(&mut state, &[(s1, 4, 1), (s2, 3, 1)], 1, options).expect("No conflict");
    state.assert_bounds(s1, 4, 4);
    state.assert_bounds(s2, 0, 8);

    for removed in 2..8 {
        assert!(!state.contains(s2, removed));
        assert_eq!(
            conjunction!([s1 <= 4] & [s1 >= 4]),
            reason_for(&mut state, predicate!(s2 != removed))
        );
    }
}

// The scenarios below return the explanations as a `Vec` rather than a `PropositionalConjunction`
// since the order of the predicates matters; without synchronisation, the explanations of the
// incremental propagators only differ in the order of the predicates.

/// Returns the explanation of the conflict found by `P` after two tasks are pushed onto a fixed
/// task; when `reverse_updates` is true, the bounds are updated in the opposite order.
fn conflict_after_updates<P: TimeTableTestCase>(reverse_updates: bool) -> Vec<Predicate> {
    let mut state = State::default();
    let s1 = state.new_interval_variable(5, 5, None);
    let s2 = state.new_interval_variable(1, 10, None);
    let s3 = state.new_interval_variable(1, 10, None);
    add_propagator::<P>(
        &mut state,
        &[(s1, 2, 1), (s2, 4, 1), (s3, 4, 1)],
        1,
        naive_options(),
    )
    .expect("No conflict");

    let mut updates = [predicate!(s3 >= 7), predicate!(s2 >= 7)];
    if reverse_updates {
        updates.reverse();
    }
    for update in updates {
        let _ = state.post(update).expect("No empty domain");
    }
    conflict_explanation(state.propagate_to_fixed_point())
        .iter()
        .copied()
        .collect()
}

/// Returns the reason for the lower-bound propagation which `P` performs after two updates; when
/// `propagate_in_between` is true, `P` propagates after each update.
fn explanation_after_updates<P: TimeTableTestCase>(propagate_in_between: bool) -> Vec<Predicate> {
    let mut state = State::default();
    let s1 = state.new_interval_variable(1, 6, None);
    let s2 = state.new_interval_variable(1, 6, None);
    let s3 = state.new_interval_variable(5, 11, None);
    add_propagator::<P>(
        &mut state,
        &[(s1, 2, 1), (s2, 4, 1), (s3, 4, 1)],
        2,
        naive_options(),
    )
    .expect("No conflict");

    let _ = state.post(predicate!(s2 >= 5)).expect("No empty domain");
    if propagate_in_between {
        state.propagate_to_fixed_point().expect("No conflict");
    }
    let _ = state.post(predicate!(s1 >= 5)).expect("No empty domain");
    state.propagate_to_fixed_point().expect("No conflict");
    assert_eq!(state.lower_bound(s3), 7);

    reason_for(&mut state, predicate!(s3 >= 7))
        .iter()
        .copied()
        .collect()
}

/// Returns the explanation of the conflict found by `P` after a task is fixed onto two others.
fn conflict_after_fixing<P: TimeTableTestCase>() -> Vec<Predicate> {
    let mut state = State::default();
    let s0 = state.new_interval_variable(1, 11, None);
    let s1 = state.new_interval_variable(1, 5, None);
    let s2 = state.new_interval_variable(1, 5, None);
    add_propagator::<P>(
        &mut state,
        &[(s0, 4, 1), (s1, 1, 1), (s2, 1, 1)],
        1,
        naive_options(),
    )
    .expect("No conflict");

    for update in [
        predicate!(s2 >= 5),
        predicate!(s1 >= 5),
        predicate!(s0 >= 5),
        predicate!(s0 <= 5),
    ] {
        let _ = state.post(update).expect("No empty domain");
    }
    conflict_explanation(state.propagate_to_fixed_point())
        .iter()
        .copied()
        .collect()
}

/// Instantiates the shared tests for each provided propagator type, in a module with the given
/// name.
macro_rules! time_table_tests {
    ($($module:ident: $propagator:ty),* $(,)?) => {
        $(
            mod $module {
                use super::*;

                #[test]
                fn propagator_propagates_from_profile() {
                    super::propagator_propagates_from_profile::<$propagator>()
                }

                #[test]
                fn propagator_detects_conflict() {
                    super::propagator_detects_conflict::<$propagator>()
                }

                #[test]
                fn propagator_propagates_nothing() {
                    super::propagator_propagates_nothing::<$propagator>()
                }

                #[test]
                fn propagator_propagates_example_4_3_schutt() {
                    super::propagator_propagates_example_4_3_schutt::<$propagator>()
                }

                #[test]
                fn propagator_propagates_after_assignment() {
                    super::propagator_propagates_after_assignment::<$propagator>()
                }

                #[test]
                fn propagator_propagates_end_time() {
                    super::propagator_propagates_end_time::<$propagator>()
                }

                #[test]
                fn propagator_propagates_example_4_3_schutt_after_update() {
                    super::propagator_propagates_example_4_3_schutt_after_update::<$propagator>()
                }

                #[test]
                fn propagator_propagates_example_4_3_schutt_multiple_profiles() {
                    super::propagator_propagates_example_4_3_schutt_multiple_profiles::<$propagator>()
                }

                #[test]
                fn propagator_propagates_from_profile_reason() {
                    super::propagator_propagates_from_profile_reason::<$propagator>()
                }

                #[test]
                fn propagator_propagates_generic_bounds() {
                    super::propagator_propagates_generic_bounds::<$propagator>()
                }

                #[test]
                fn propagator_propagates_with_holes() {
                    super::propagator_propagates_with_holes::<$propagator>()
                }
            }
        )*
    };
}

/// Instantiates the tests comparing an incremental propagator (with and without synchronisation)
/// against its non-incremental counterpart, in a module with the given name.
macro_rules! synchronisation_tests {
    ($($module:ident: $scratch:ident, $incremental:ident);* $(;)?) => {
        $(
            mod $module {
                use super::*;

                type Scratch = $scratch<DomainId>;
                type Synced = $incremental<DomainId, true>;
                type Unsynced = $incremental<DomainId, false>;

                #[test]
                fn synchronisation_leads_to_same_conflict_explanation() {
                    // The incremental propagator is notified in a different order than the
                    // scratch propagator
                    assert_eq!(
                        conflict_after_updates::<Scratch>(true),
                        conflict_after_updates::<Synced>(false)
                    );
                }

                #[test]
                fn no_synchronisation_leads_to_different_conflict_explanation() {
                    assert_ne!(
                        conflict_after_updates::<Scratch>(false),
                        conflict_after_updates::<Unsynced>(false)
                    );
                }

                #[test]
                fn synchronisation_leads_to_same_explanation() {
                    assert_eq!(
                        explanation_after_updates::<Scratch>(false),
                        explanation_after_updates::<Synced>(true)
                    );
                }

                #[test]
                fn no_synchronisation_leads_to_different_explanation() {
                    assert_ne!(
                        explanation_after_updates::<Scratch>(false),
                        explanation_after_updates::<Unsynced>(true)
                    );
                }

                #[test]
                fn synchronisation_leads_to_same_conflict() {
                    assert_eq!(
                        conflict_after_fixing::<Scratch>(),
                        conflict_after_fixing::<Synced>()
                    );
                }
            }
        )*
    };
}

time_table_tests! {
    per_point: TimeTablePerPointPropagator<DomainId>,
    over_interval: TimeTableOverIntervalPropagator<DomainId>,
    per_point_incremental: TimeTablePerPointIncrementalPropagator<DomainId, false>,
    per_point_incremental_synchronised: TimeTablePerPointIncrementalPropagator<DomainId, true>,
    over_interval_incremental: TimeTableOverIntervalIncrementalPropagator<DomainId, false>,
    over_interval_incremental_synchronised: TimeTableOverIntervalIncrementalPropagator<DomainId, true>,
}

synchronisation_tests! {
    per_point_synchronisation: TimeTablePerPointPropagator, TimeTablePerPointIncrementalPropagator;
    over_interval_synchronisation: TimeTableOverIntervalPropagator, TimeTableOverIntervalIncrementalPropagator;
}
