use std::rc::Rc;

use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::ReadDomains;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;
use crate::propagators::ResourceProfile;
use crate::propagators::Task;
use crate::variables::IntegerVariable;

/// Creates the propagation explanation using the naive approach (see
/// [`CumulativeExplanationType::Naive`])
pub(crate) fn create_naive_propagation_explanation<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
>(
    profile: &ResourceProfile<Var, PVar, RVar>,
    context: PropagationContext,
    capacity: CVar,
) -> PropositionalConjunction {
    profile
        .profile_tasks
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable
                        >= context.lower_bound(&profile_task.start_variable)
                ),
                predicate!(
                    profile_task.start_variable
                        <= context.upper_bound(&profile_task.start_variable)
                ),
                predicate!(
                    profile_task.processing_time
                        >= context.lower_bound(&profile_task.processing_time)
                ),
                predicate!(
                    profile_task.resource_usage
                        >= context.lower_bound(&profile_task.resource_usage)
                ),
                predicate!(capacity <= context.upper_bound(&capacity)),
            ]
        })
        .filter(|&predicate| predicate != Predicate::trivially_true())
        .collect()
}

/// Creates the conflict explanation using the naive approach (see
/// [`CumulativeExplanationType::Naive`])
pub(crate) fn create_naive_conflict_explanation<
    Var: IntegerVariable + 'static,
    PVar: IntegerVariable + 'static,
    RVar: IntegerVariable + 'static,
    CVar: IntegerVariable + 'static,
    Context: ReadDomains + Copy,
>(
    conflict_profile: &ResourceProfile<Var, PVar, RVar>,
    context: Context,
    capacity: CVar,
) -> PropositionalConjunction {
    conflict_profile
        .profile_tasks
        .iter()
        .flat_map(|profile_task| {
            [
                predicate!(
                    profile_task.start_variable
                        >= context.lower_bound(&profile_task.start_variable)
                ),
                predicate!(
                    profile_task.start_variable
                        <= context.upper_bound(&profile_task.start_variable)
                ),
                predicate!(
                    profile_task.processing_time
                        >= context.lower_bound(&profile_task.processing_time)
                ),
                predicate!(
                    profile_task.resource_usage
                        >= context.lower_bound(&profile_task.resource_usage)
                ),
                predicate!(capacity <= context.upper_bound(&capacity)),
            ]
        })
        .filter(|&predicate| predicate != Predicate::trivially_true())
        .collect()
}

pub(crate) fn create_naive_predicate_propagating_task_lower_bound_propagation<Var, PVar, RVar>(
    context: PropagationContext,
    task: &Rc<Task<Var, PVar, RVar>>,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(task.start_variable >= context.lower_bound(&task.start_variable))
}

pub(crate) fn create_naive_predicate_propagating_task_upper_bound_propagation<Var, PVar, RVar>(
    context: PropagationContext,
    task: &Rc<Task<Var, PVar, RVar>>,
) -> Predicate
where
    Var: IntegerVariable + 'static,
{
    predicate!(task.start_variable <= context.upper_bound(&task.start_variable))
}

#[cfg(test)]
mod tests {
    use crate::constraint_arguments::CumulativeExplanationType;
    use crate::predicate;
    use crate::predicates::PropositionalConjunction;
    use crate::propagators::cumulative::time_table::propagation_handler::test_propagation_handler::TestPropagationHandler;

    #[test]
    fn test_naive_explanation_lower_bound() {
        let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
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
    fn test_naive_explanation_lower_bound_sequence() {
        let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
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
    fn test_naive_explanation_upper_bound() {
        let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
        let (reason, x, y) = propagation_handler.set_up_example_upper_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 16),
            predicate!(y >= 15),
            predicate!(y <= 16),
        ]
        .into();
        assert_eq!(reason, expected_reason);
    }

    #[test]
    fn test_naive_explanation_upper_bound_sequence() {
        let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
        let (reason, x, y, z) = propagation_handler.set_up_example_sequence_upper_bound();
        let expected_reason: PropositionalConjunction = vec![
            predicate!(x <= 16),
            predicate!(y >= 15),
            predicate!(y <= 16),
            predicate!(z >= 7),
            predicate!(z <= 9),
        ]
        .into();
        assert_eq!(reason, expected_reason);
    }

    #[test]
    fn test_conflict_naive() {
        let mut propagation_handler = TestPropagationHandler::new(CumulativeExplanationType::Naive);
        let (reason, y) = propagation_handler.set_up_conflict_example();
        let expected_reason: PropositionalConjunction =
            vec![predicate!(y >= 15), predicate!(y <= 16)].into();
        assert_eq!(reason, expected_reason);
    }
}
