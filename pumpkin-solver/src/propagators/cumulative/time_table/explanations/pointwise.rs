use std::rc::Rc;

use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::pumpkin_assert_simple;
use crate::variables::IntegerVariable;

/// Creates the propagation explanation using the point-wise approach (see
/// [`CumulativeExplanationType::PointWise`])
pub(crate) fn create_pointwise_propagation_explanation<Var: IntegerVariable + 'static>(
    time_point: i32,
    profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    profile
        .profile_tasks
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
pub(crate) fn create_pointwise_conflict_explanation<Var: IntegerVariable + 'static>(
    conflict_profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    // As stated in improving scheduling by learning, we choose the middle point; this
    // could potentially be improved
    let middle_point = (conflict_profile.end - conflict_profile.start) / 2 + conflict_profile.start;
    pumpkin_assert_simple!(
        middle_point >= conflict_profile.start && middle_point <= conflict_profile.end
    );

    conflict_profile
        .profile_tasks
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
            TestPropagationHandler::new(CumulativeExplanationType::PointWise);
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
            TestPropagationHandler::new(CumulativeExplanationType::PointWise);
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
            TestPropagationHandler::new(CumulativeExplanationType::PointWise);
        let (reason_last_propagation, x, y) = propagation_handler.set_up_example_upper_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 16),
            predicate!(y >= 13),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason_last_propagation, expected_reason);

        let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x <= 12));
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 18),
            predicate!(y >= 15),
            predicate!(y <= 18),
        ]
        .into();
        assert_eq!(reason_first_propagation, expected_reason);
    }

    #[test]
    fn test_pointwise_explanation_upper_bound_sequence() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::PointWise);
        let (reason_last_propagation, x, y, z) =
            propagation_handler.set_up_example_sequence_upper_bound();
        let expected_reason: PropositionalConjunction =
            vec![predicate!(x <= 9), predicate!(z >= 4), predicate!(z <= 9)].into();
        assert_eq!(reason_last_propagation, expected_reason);

        let reason_middle_propagation = propagation_handler.get_reason_for(predicate!(x <= 6));
        let expected_reason: PropositionalConjunction =
            vec![predicate!(x <= 12), predicate!(z >= 7), predicate!(z <= 12)].into();
        assert_eq!(reason_middle_propagation, expected_reason);

        let reason_first_propagation = propagation_handler.get_reason_for(predicate!(x <= 12));
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 18),
            predicate!(y >= 15),
            predicate!(y <= 18),
        ]
        .into();
        assert_eq!(reason_first_propagation, expected_reason);
    }

    #[test]
    fn test_conflict_point_wise() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::PointWise);
        let (reason, y) = propagation_handler.set_up_conflict_example();
        let expected_reason: PropositionalConjunction =
            vec![predicate!(y >= 13), predicate!(y <= 16)].into();
        assert_eq!(reason, expected_reason);
    }
}
