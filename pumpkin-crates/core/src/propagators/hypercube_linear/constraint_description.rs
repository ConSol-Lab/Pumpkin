use crate::checkers::CompleteSolution;
use crate::checkers::ScopeItem;
use crate::propagation::ConstraintDescription;
use crate::propagation::SolutionCheck;
use crate::propagators::hypercube_linear::Hypercube;
use crate::propagators::hypercube_linear::LinearInequality;

/// The description of the hypercube linear constraint: when every predicate of the hypercube
/// holds, the linear inequality holds.
#[derive(Clone, Debug, ScopeItem)]
pub struct HypercubeLinearDescription {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
}

impl ConstraintDescription for HypercubeLinearDescription {
    fn check_solution(&self, solution: &CompleteSolution<'_>) -> SolutionCheck {
        let is_hypercube_satisfied = self
            .hypercube
            .iter_predicates()
            .all(|predicate| solution.predicate_holds(predicate));
        let sum = self
            .linear
            .terms()
            .map(|term| i64::from(solution.integer_value(&term)))
            .sum::<i64>();

        if !is_hypercube_satisfied || sum <= i64::from(self.linear.bound()) {
            SolutionCheck::ConstraintSatisfied
        } else {
            SolutionCheck::ConstraintViolated
        }
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZero;

    use super::*;
    use crate::predicate;
    use crate::state::State;

    #[test]
    fn the_linear_inequality_only_has_to_hold_inside_the_hypercube() {
        let mut state = State::default();
        let x = state.new_interval_variable(0, 10, None);
        let y = state.new_interval_variable(0, 10, None);
        let _ = state
            .post(predicate![y == 6])
            .expect("the value is in the domain");

        // [x >= 5] -> y <= 4
        let description = HypercubeLinearDescription {
            hypercube: Hypercube::new([predicate![x >= 5]]).expect("not inconsistent"),
            linear: LinearInequality::new([(NonZero::new(1).unwrap(), y)], 4)
                .expect("not trivially true"),
        };

        state.new_checkpoint();
        let _ = state
            .post(predicate![x == 7])
            .expect("the value is in the domain");
        assert_eq!(
            description
                .check_solution(&state.complete_solution().expect("every variable is fixed")),
            SolutionCheck::ConstraintViolated
        );

        let _ = state.restore_to(0);
        let _ = state
            .post(predicate![x == 2])
            .expect("the value is in the domain");
        assert_eq!(
            description
                .check_solution(&state.complete_solution().expect("every variable is fixed")),
            SolutionCheck::ConstraintSatisfied
        );
    }
}
