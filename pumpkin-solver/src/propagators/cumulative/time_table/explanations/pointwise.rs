use std::rc::Rc;

use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::propagation_context::HasAssignments;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::EmptyDomain;
use crate::options::CumulativeExplanationType;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::cumulative::time_table::explanations::add_propagating_task_predicate_lower_bound;
use crate::propagators::cumulative::time_table::explanations::add_propagating_task_predicate_upper_bound;
use crate::propagators::ResourceProfileInterface;
use crate::propagators::Task;
use crate::pumpkin_assert_extreme;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

pub(crate) fn propagate_lower_bounds_with_pointwise_explanations<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    context: &mut PropagationContextMut,
    profiles: &[&ResourceProfileType],
    propagating_task: &Rc<Task<Var>>,
) -> Result<(), EmptyDomain> {
    // The time points should follow the following properties (based on `Improving
    // scheduling by learning - Andreas Schutt`):
    // 1. `t_0 = lb(s)`
    // 2. `t_m = profiles.last.get_end()`
    // 3. `∀1 <= j <= m : t_{j - 1} + p >= t_j`
    // 4. There exists a mapping `P(t_j)` such that `∀1 <= j <= m : P(t_j).start <= t_j
    //   <= P(t_j).get_end()`
    //
    // Property 1 ensures that the first propagation is correct
    // Property 2 ensures that the maximum propagation is reached by the set of
    // profiles
    // Property 3 ensures that every `t_i` is not more than
    // `p` units apart
    // Property 4 ensures that every time-point falls within the
    // range of a profile.
    let mut current_profile_index = 0;
    // We take as `t_1` the minimum of the first profile end and the earliest
    // completion time - 1 (this - 1 is necessary since the explanation uses the
    // predicate `[s >= t_l + 1 - p]`, and this predicate holds only if the -1 is added)
    let mut time_point = profiles[current_profile_index].get_end().min(
        context.lower_bound(&propagating_task.start_variable) + propagating_task.processing_time
            - 1,
    );
    let mut should_exit = false;

    loop {
        pumpkin_assert_simple!(
                        time_point >= profiles[current_profile_index].get_start()
                            && time_point <= profiles[current_profile_index].get_end(),
                        "The time-point ({time_point}) should have been between the start ({}) and end ({}) of the first profile!",
                        profiles[current_profile_index].get_start(),
                        profiles[current_profile_index].get_end()
                    );

        if time_point >= context.lower_bound(&propagating_task.start_variable) {
            let explanation = add_propagating_task_predicate_lower_bound(
                create_pointwise_propagation_explanation(
                    time_point,
                    profiles[current_profile_index],
                ),
                CumulativeExplanationType::Pointwise,
                context.as_readonly(),
                propagating_task,
                profiles[current_profile_index],
                Some(time_point),
            );
            pumpkin_assert_extreme!(
                explanation.iter().all(|predicate| context
                    .assignments_integer()
                    .does_integer_predicate_hold((*predicate).try_into().unwrap())),
                "All of the predicates in the reason should hold"
            );
            context.set_lower_bound(
                &propagating_task.start_variable,
                time_point + 1,
                explanation,
            )?;
        }

        if should_exit {
            break;
        }

        // We place the time-point as far as possible
        time_point += propagating_task.processing_time;

        // Then we update the index of the current profile if appropriate
        if time_point > profiles[current_profile_index].get_end() {
            if current_profile_index < profiles.len() - 1
                && time_point < profiles[current_profile_index + 1].get_start()
            {
                // The time-point has ended up between profiles, we thus set the
                // time-point to the end of the current profile and propagate from
                // there
                //
                // (Note that we could have also set it to
                // `profiles[current_profile_index + 1].start -
                // propagating_task.processing_time`)
                time_point = profiles[current_profile_index].get_end();
            } else {
                current_profile_index += 1;
            }
        }

        // We have gone past the last profile, we ensure that we propagate past its end
        // point here
        if current_profile_index >= profiles.len() {
            current_profile_index -= 1;
            time_point = profiles[current_profile_index].get_end();
            should_exit = true;
            continue;
        }

        // Now we check whether we are skipping a profile, if this is the case then we
        // set the time-point to the end of the next profile rather than skipping it
        // entirely (this is preferable according to `Improving Scheduling by
        // Learning`).
        if time_point > profiles[current_profile_index].get_end() {
            time_point = profiles[current_profile_index].get_end()
        }
    }
    Ok(())
}
pub(crate) fn propagate_upper_bounds_with_pointwise_explanations<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    context: &mut PropagationContextMut,
    profiles: &[&ResourceProfileType],
    propagating_task: &Rc<Task<Var>>,
) -> Result<(), EmptyDomain> {
    // The time points should follow the following properties (based on `Improving
    // scheduling by learning - Andreas Schutt`):
    // 1. `t_0 = ub(s) + p`
    // 2. `t_m = profiles.first.start`
    // 3. `∀1 <= j <= m : t_{j - 1} - p >= t_j`
    // 4. There exists a mapping `P(t_j)` such that `∀1 <= j <= m : P(t_j).start <= t_j
    //   <= P(t_j).get_end()`
    //
    // Property 1 ensures that the first propagation is correct
    // Property 2 ensures that the maximum propagation is reached by the set of
    // profiles
    // Property 3 ensures that every `t_i` is not more than
    // `p` units apart
    // Property 4 ensures that every time-point falls within the
    // range of a profile.
    let mut current_profile_index = profiles.len() - 1;
    // We take as `t_1` the maximum of the last profile start and the
    // latest start time
    let mut time_point = profiles[current_profile_index]
        .get_start()
        .max(context.upper_bound(&propagating_task.start_variable));
    let mut should_exit = false;

    loop {
        pumpkin_assert_simple!(
                        time_point >= profiles[current_profile_index].get_start()
                            && time_point <= profiles[current_profile_index].get_end(),
                        "The time-point ({time_point}) should have been between the start ({}) and end ({}) of the first profile!",
                        profiles[current_profile_index].get_start(),
                        profiles[current_profile_index].get_end()
                    );

        if time_point - propagating_task.processing_time
            < context.upper_bound(&propagating_task.start_variable)
        {
            let explanation = add_propagating_task_predicate_upper_bound(
                create_pointwise_propagation_explanation(
                    time_point,
                    profiles[current_profile_index],
                ),
                CumulativeExplanationType::Pointwise,
                context.as_readonly(),
                propagating_task,
                profiles[current_profile_index],
                Some(time_point),
            );
            pumpkin_assert_extreme!(
                explanation.iter().all(|predicate| context
                    .assignments_integer()
                    .does_integer_predicate_hold((*predicate).try_into().unwrap())),
                "All of the predicates in the reason should hold"
            );
            context.set_upper_bound(
                &propagating_task.start_variable,
                time_point - propagating_task.processing_time,
                explanation,
            )?;
        }

        if should_exit {
            break;
        }

        time_point -= propagating_task.processing_time;

        // Then we update the index of the current profile if appropriate
        if time_point < profiles[current_profile_index].get_start() {
            if current_profile_index > 0
                && time_point > profiles[current_profile_index - 1].get_end()
            {
                // The time-point has ended up between profiles, we thus set the
                // time-point to the start of the current profile and propagate from
                // there
                //
                // (Note that we could have also set it to
                // `profiles[current_profile_index - 1].get_end() +
                // propagating_task.processing_time`)
                time_point = profiles[current_profile_index].get_start()
            } else if current_profile_index == 0 {
                // We have gone past the first profile, we ensure that we propagate past
                // its start point here
                time_point = profiles[current_profile_index].get_start();
                should_exit = true;
                continue;
            } else {
                current_profile_index -= 1;
            }
        }

        // Now we check whether we are skipping a profile, if this is the case then we
        // set the time-point to the end of the next profile rather than skipping it
        // entirely (this is preferable according to `Improving Scheduling by
        // Learning`).
        if time_point < profiles[current_profile_index].get_start() {
            time_point = profiles[current_profile_index].get_start()
        }
    }
    Ok(())
}

/// Creates the propagation explanation using the point-wise approach (see
/// [`CumulativeExplanationType::PointWise`])
pub(crate) fn create_pointwise_propagation_explanation<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    time_point: i32,
    profile: &ResourceProfileType,
) -> PropositionalConjunction {
    profile
        .get_profile_tasks()
        .iter()
        .flat_map(move |profile_task| {
            [
                predicate!(
                    profile_task.start_variable >= time_point + 1 - profile_task.processing_time
                ),
                predicate!(profile_task.start_variable <= time_point),
            ]
        })
        .collect()
}

/// Creates the conflict explanation using the point-wise approach (see
/// [`CumulativeExplanationType::PointWise`])
pub(crate) fn create_pointwise_conflict_explanation<
    Var: IntegerVariable + 'static,
    ResourceProfileType: ResourceProfileInterface<Var>,
>(
    conflict_profile: &ResourceProfileType,
) -> PropositionalConjunction {
    // As stated in improving scheduling by learning, we choose the middle point; this
    // could potentially be improved
    let middle_point = (conflict_profile.get_end() - conflict_profile.get_start()) / 2
        + conflict_profile.get_start();
    pumpkin_assert_simple!(
        middle_point >= conflict_profile.get_start() && middle_point <= conflict_profile.get_end()
    );

    conflict_profile
        .get_profile_tasks()
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable >= middle_point + 1 - profile_task.processing_time
                ),
                predicate!(profile_task.start_variable <= middle_point),
            ]
        })
        .collect()
}

pub(crate) fn create_pointwise_predicate_propagating_task_lower_bound_propagation<Var>(
    task: &Rc<Task<Var>>,
    time_point: Option<i32>,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(
        task.start_variable
            >= time_point
                .expect("Expected time-point to be provided to pointwise explanation creation")
                + 1
                - task.processing_time
    )
}

pub(crate) fn create_pointwise_predicate_propagating_task_upper_bound_propagation<Var>(
    task: &Rc<Task<Var>>,
    time_point: Option<i32>,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(
        task.start_variable
            <= time_point
                .expect("Expected time-point to be provided to pointwise explanation creation")
    )
}

#[cfg(test)]
mod tests {
    use crate::options::CumulativeExplanationType;
    use crate::predicate;
    use crate::predicates::PropositionalConjunction;
    use crate::propagators::cumulative::time_table::propagation_handler::test_propagation_handler::TestPropagationHandler;

    #[test]
    fn test_pointwise_explanation_lower_bound() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
        let (reason_last_propagation, x, y) = propagation_handler.set_up_example_lower_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x >= 13),
            predicate!(y >= 15),
            predicate!(y <= 18),
        ]
        .into();
        assert_eq!(reason_last_propagation, expected_reason);

        let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x >= 17));
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x >= 11),
            predicate!(y >= 13),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason_first_propagation, expected_reason);
    }

    #[test]
    fn test_pointwise_explanation_lower_bound_sequence() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
        let (reason_last_propagation, x, y, z) =
            propagation_handler.set_up_example_sequence_lower_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x >= 16),
            predicate!(z >= 15),
            predicate!(z <= 21),
        ]
        .into();
        assert_eq!(reason_last_propagation, expected_reason);

        let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x >= 17));
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x >= 11),
            predicate!(y >= 13),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason_first_propagation, expected_reason);
    }

    #[test]
    fn test_pointwise_explanation_upper_bound() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
        let (reason_last_propagation, x, y) = propagation_handler.set_up_example_upper_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 16),
            predicate!(y >= 13),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason_last_propagation, expected_reason);
    }

    #[test]
    fn test_pointwise_explanation_upper_bound_sequence() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
        let (reason_last_propagation, x, y, z) =
            propagation_handler.set_up_example_sequence_upper_bound();
        let expected_reason: PropositionalConjunction =
            vec![predicate!(x <= 9), predicate!(z >= 4), predicate!(z <= 9)].into();
        assert_eq!(reason_last_propagation, expected_reason);

        let reason_middle_propagation = propagation_handler.get_reason_for(predicate!(x <= 4));
        let expected_reason: PropositionalConjunction =
            vec![predicate!(x <= 10), predicate!(z >= 5), predicate!(z <= 10)].into();
        assert_eq!(reason_middle_propagation, expected_reason);

        let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x <= 10));
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 16),
            predicate!(y >= 13),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason_first_propagation, expected_reason);
    }

    #[test]
    fn test_conflict_point_wise() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::Pointwise);
        let (reason, y) = propagation_handler.set_up_conflict_example();
        let expected_reason: PropositionalConjunction =
            vec![predicate!(y >= 13), predicate!(y <= 16)].into();
        assert_eq!(reason, expected_reason);
    }
}
