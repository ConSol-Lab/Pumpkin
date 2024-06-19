use super::pseudo_boolean_constraint_encoder::EncodingError;
use super::PseudoBooleanConstraintEncoderInterface;
use crate::basic_types::WeightedLiteral;
use crate::engine::ConstraintSatisfactionSolver;
use crate::pumpkin_assert_simple;

#[derive(Debug)]
pub struct SingleIntegerEncoder {
    index_last_added_weighted_literal: usize,
    weighted_literals: Vec<WeightedLiteral>,
}

impl PseudoBooleanConstraintEncoderInterface for SingleIntegerEncoder {
    fn encode_at_most_k(
        mut weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<Self, EncodingError>
    where
        Self: Sized,
    {
        weighted_literals.sort_by(|a, b| a.bound.unwrap().cmp(&b.bound.unwrap()));
        let mut encoder = SingleIntegerEncoder {
            index_last_added_weighted_literal: usize::MAX,
            weighted_literals,
        };
        encoder.strengthen_at_most_k(k, csp_solver)?;
        Ok(encoder)
    }

    fn strengthen_at_most_k(
        &mut self,
        k: u64,
        csp_solver: &mut ConstraintSatisfactionSolver,
    ) -> Result<(), EncodingError> {
        pumpkin_assert_simple!(self.index_last_added_weighted_literal > 0);
        self.index_last_added_weighted_literal = self.weighted_literals.len();

        for i in (0..self.index_last_added_weighted_literal).rev() {
            if self.weighted_literals[i].bound.expect(
                "The single integer case should only be provided with literals which have bounds",
            ) > k as i32
            {
                self.index_last_added_weighted_literal = i;
                if csp_solver
                    .add_clause([!self.weighted_literals[i].literal])
                    .is_err()
                {
                    return Err(EncodingError::CannotStrengthen);
                }
            } else {
                break;
            }
        }
        self.weighted_literals
            .truncate(self.index_last_added_weighted_literal);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::SingleIntegerEncoder;
    use crate::basic_types::WeightedLiteral;
    use crate::encoders::pseudo_boolean_constraint_encoder::PseudoBooleanConstraintEncoderInterface;
    use crate::engine::variables::DomainId;
    use crate::engine::ConstraintSatisfactionSolver;
    use crate::predicate;

    fn weighted_literals(
        csp_solver: &mut ConstraintSatisfactionSolver,
        lower_bound: i32,
        upper_bound: i32,
        domain: DomainId,
        weight: u64,
    ) -> Vec<WeightedLiteral> {
        ((lower_bound + 1)..=upper_bound)
            .map(|i| {
                let literal = csp_solver.get_literal(predicate![domain >= i]);
                WeightedLiteral {
                    literal,
                    weight,
                    bound: Some(i),
                }
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn test_valid_encode_at_most_k_returns_encoder() {
        let (lower_bound, upper_bound) = (0, 10);
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let domain = csp_solver.create_new_integer_variable(lower_bound, upper_bound);

        let weight = 1;
        let k = 5;
        let weighted_literals =
            weighted_literals(&mut csp_solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut csp_solver);
        assert!(result.is_ok());
        assert!((k + 1..=upper_bound as u64).all(|lower_bound| csp_solver
            .get_literal_value(csp_solver.get_literal(predicate![domain >= lower_bound as i32]))
            == Some(false)));
    }

    #[test]
    fn test_invalid_encode_at_most_k_returns_err() {
        let (lower_bound, upper_bound) = (0, 10);
        let k: u64 = 5;
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let domain = csp_solver.create_new_integer_variable(lower_bound, upper_bound);
        let _ = csp_solver.add_clause([csp_solver.get_literal(predicate![domain >= k as i32 + 1])]);

        let weight = 1;
        let weighted_literals =
            weighted_literals(&mut csp_solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut csp_solver);
        assert!(result.is_err())
    }

    #[test]
    fn test_encoding_with_k_higher_than_upper_bound_results_in_encoder() {
        let (lower_bound, upper_bound) = (0, 10);
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let domain = csp_solver.create_new_integer_variable(lower_bound, upper_bound);

        let weight = 1;
        let k = 15;
        let weighted_literals =
            weighted_literals(&mut csp_solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut csp_solver);
        assert!(result.is_ok());
        assert!(
            ((lower_bound as u64 + 1)..=upper_bound as u64).all(|lower_bound| csp_solver
                .get_literal_value(csp_solver.get_literal(predicate![domain >= lower_bound as i32]))
                .is_none())
        );
    }

    #[test]
    fn test_valid_strengthen_at_most_k_returns_ok() {
        let (lower_bound, upper_bound) = (0, 10);
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let domain = csp_solver.create_new_integer_variable(lower_bound, upper_bound);

        let weight = 1;
        let k = 15;
        let weighted_literals =
            weighted_literals(&mut csp_solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut csp_solver);
        assert!(result.is_ok());
        let mut encoder = result.unwrap();
        let k = 5;
        let result = encoder.strengthen_at_most_k(5, &mut csp_solver);
        assert!(result.is_ok());
        assert!((k + 1..=upper_bound as u64).all(|lower_bound| csp_solver
            .get_literal_value(csp_solver.get_literal(predicate![domain >= lower_bound as i32]))
            == Some(false)));
    }

    #[test]
    fn test_invalid_strengthen_at_most_k_returns_err() {
        let (lower_bound, upper_bound) = (0, 10);
        let mut csp_solver = ConstraintSatisfactionSolver::default();
        let domain = csp_solver.create_new_integer_variable(lower_bound, upper_bound);

        let weight = 1;
        let k = 15;
        let weighted_literals =
            weighted_literals(&mut csp_solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut csp_solver);
        assert!(result.is_ok());
        let mut encoder = result.unwrap();
        let k = 5;
        let _ = csp_solver.add_clause([csp_solver.get_literal(predicate![domain >= k + 1])]);
        let result = encoder.strengthen_at_most_k(5, &mut csp_solver);
        assert!(result.is_err());
    }
}
