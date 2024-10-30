use std::cmp::max;
use std::rc::Rc;

use crate::engine::cp::propagation::propagation_context::ReadDomains;
use crate::engine::propagation::PropagationContext;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Creates the propagation explanation using the big-step approach (see
/// [`CumulativeExplanationType::BigStep`])
pub(crate) fn create_big_step_propagation_explanation<Var: IntegerVariable + 'static>(
    profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    profile
        .profile_tasks
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable >= profile.end - profile_task.processing_time + 1
                ),
                predicate!(profile_task.start_variable <= profile.start),
            ]
        })
        .collect()
}

/// Creates the conflict explanation using the big-step approach (see
/// [`CumulativeExplanationType::BigStep`])
pub(crate) fn create_big_step_conflict_explanation<Var: IntegerVariable + 'static>(
    conflict_profile: &ResourceProfile<Var>,
) -> PropositionalConjunction {
    conflict_profile
        .profile_tasks
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable
                        >= conflict_profile.end - profile_task.processing_time + 1
                ),
                predicate!(profile_task.start_variable <= conflict_profile.start),
            ]
        })
        .collect()
}

pub(crate) fn create_big_step_predicate_propagating_task_lower_bound_propagation<Var>(
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(task.start_variable >= profile.start + 1 - task.processing_time)
}

pub(crate) fn create_big_step_predicate_propagating_task_upper_bound_propagation<Var>(
    task: &Rc<Task<Var>>,
    profile: &ResourceProfile<Var>,
    context: PropagationContext,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(task.start_variable <= max(context.upper_bound(&task.start_variable), profile.end))
}

#[cfg(test)]
mod tests {
    use crate::options::CumulativeExplanationType;
    use crate::predicate;
    use crate::predicates::PropositionalConjunction;
    use crate::propagators::cumulative::time_table::propagation_handler::test_propagation_handler::TestPropagationHandler;

    #[test]
    fn test_big_step_explanation_lower_bound() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::BigStep);
        let (reason, x, y) = propagation_handler.set_up_example_lower_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x >= 11),
            predicate!(y >= 15),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason, expected_reason);
    }

    #[test]
    fn test_big_step_explanation_lower_bound_sequence() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::BigStep);
        let (reason, x, y, z) = propagation_handler.set_up_example_sequence_lower_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x >= 11),
            predicate!(y >= 15),
            predicate!(y <= 16),
            predicate!(z >= 15),
            predicate!(z <= 19),
        ]
        .into();
        assert_eq!(reason, expected_reason);
    }

    #[test]
    fn test_big_step_explanation_upper_bound() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::BigStep);
        let (reason, x, y) = propagation_handler.set_up_example_upper_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 18),
            predicate!(y >= 15),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason, expected_reason);
    }

    #[test]
    fn test_big_step_explanation_upper_bound_sequence() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::BigStep);
        let (reason, x, y, z) = propagation_handler.set_up_example_sequence_upper_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 18),
            predicate!(y >= 15),
            predicate!(y <= 16),
            predicate!(z >= 7),
            predicate!(z <= 9),
        ]
        .into();
        assert_eq!(reason, expected_reason);
    }

    #[test]
    fn test_conflict_big_step() {
        let mut propagation_handler =
            TestPropagationHandler::new(CumulativeExplanationType::BigStep);
        let (reason, y) = propagation_handler.set_up_conflict_example();
        let expected_reason: PropositionalConjunction =
            vec![predicate!(y >= 14), predicate!(y <= 15)].into();
        assert_eq!(reason, expected_reason);
    }
}
