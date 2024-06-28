use super::pseudo_boolean_constraint_encoder::EncodingError;
use super::PseudoBooleanConstraintEncoderInterface;
use crate::basic_types::WeightedLiteral;
use crate::pumpkin_assert_simple;
use crate::Solver;

/// An encoder which takes as input a single integer encoding.
///
/// Note that if this case occurs, we would recommend using [`Solver::maximise`] or
/// [Solver::minimise] directly.
#[derive(Debug)]
pub(crate) struct SingleIntegerEncoder {
    index_last_added_weighted_literal: usize,
    weighted_literals: Vec<WeightedLiteral>,
}

impl PseudoBooleanConstraintEncoderInterface for SingleIntegerEncoder {
    fn encode_at_most_k(
        mut weighted_literals: Vec<WeightedLiteral>,
        k: u64,
        solver: &mut Solver,
    ) -> Result<Self, EncodingError>
    where
        Self: Sized,
    {
        weighted_literals.sort_by(|a, b| a.bound.unwrap().cmp(&b.bound.unwrap()));
        let mut encoder = SingleIntegerEncoder {
            index_last_added_weighted_literal: usize::MAX,
            weighted_literals,
        };
        encoder.strengthen_at_most_k(k, solver)?;
        Ok(encoder)
    }

    fn strengthen_at_most_k(&mut self, k: u64, solver: &mut Solver) -> Result<(), EncodingError> {
        pumpkin_assert_simple!(self.index_last_added_weighted_literal > 0);
        self.index_last_added_weighted_literal = self.weighted_literals.len();

        for i in (0..self.index_last_added_weighted_literal).rev() {
            if self.weighted_literals[i].bound.expect(
                "The single integer case should only be provided with literals which have bounds",
            ) > k as i32
            {
                self.index_last_added_weighted_literal = i;
                if solver
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
    use crate::predicate;
    use crate::Solver;

    fn weighted_literals(
        solver: &mut Solver,
        lower_bound: i32,
        upper_bound: i32,
        domain: DomainId,
        weight: u64,
    ) -> Vec<WeightedLiteral> {
        ((lower_bound + 1)..=upper_bound)
            .map(|i| {
                let literal = solver.get_literal_for_predicate(predicate![domain >= i]);
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
        let mut solver = Solver::default();
        let domain = solver.new_bounded_integer(lower_bound, upper_bound);

        let weight = 1;
        let k = 5;
        let weighted_literals =
            weighted_literals(&mut solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut solver);
        assert!(result.is_ok());
        assert!(
            (k + 1..=upper_bound as u64).all(|lower_bound| solver.get_literal_value(
                solver.get_literal_for_predicate(predicate![domain >= lower_bound as i32])
            ) == Some(false))
        );
    }

    #[test]
    fn test_invalid_encode_at_most_k_returns_err() {
        let (lower_bound, upper_bound) = (0, 10);
        let k: u64 = 5;
        let mut solver = Solver::default();
        let domain = solver.new_bounded_integer(lower_bound, upper_bound);
        let _ = solver
            .add_clause([solver.get_literal_for_predicate(predicate![domain >= k as i32 + 1])]);

        let weight = 1;
        let weighted_literals =
            weighted_literals(&mut solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut solver);
        assert!(result.is_err())
    }

    #[test]
    fn test_encoding_with_k_higher_than_upper_bound_results_in_encoder() {
        let (lower_bound, upper_bound) = (0, 10);
        let mut solver = Solver::default();
        let domain = solver.new_bounded_integer(lower_bound, upper_bound);

        let weight = 1;
        let k = 15;
        let weighted_literals =
            weighted_literals(&mut solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut solver);
        assert!(result.is_ok());
        assert!(
            ((lower_bound as u64 + 1)..=upper_bound as u64).all(|lower_bound| solver
                .get_literal_value(
                    solver.get_literal_for_predicate(predicate![domain >= lower_bound as i32])
                )
                .is_none())
        );
    }

    #[test]
    fn test_valid_strengthen_at_most_k_returns_ok() {
        let (lower_bound, upper_bound) = (0, 10);
        let mut solver = Solver::default();
        let domain = solver.new_bounded_integer(lower_bound, upper_bound);

        let weight = 1;
        let k = 15;
        let weighted_literals =
            weighted_literals(&mut solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut solver);
        assert!(result.is_ok());
        let mut encoder = result.unwrap();
        let k = 5;
        let result = encoder.strengthen_at_most_k(5, &mut solver);
        assert!(result.is_ok());
        assert!(
            (k + 1..=upper_bound as u64).all(|lower_bound| solver.get_literal_value(
                solver.get_literal_for_predicate(predicate![domain >= lower_bound as i32])
            ) == Some(false))
        );
    }

    #[test]
    fn test_invalid_strengthen_at_most_k_returns_err() {
        let (lower_bound, upper_bound) = (0, 10);
        let mut solver = Solver::default();
        let domain = solver.new_bounded_integer(lower_bound, upper_bound);

        let weight = 1;
        let k = 15;
        let weighted_literals =
            weighted_literals(&mut solver, lower_bound, upper_bound, domain, weight);

        let result = SingleIntegerEncoder::encode_at_most_k(weighted_literals, k, &mut solver);
        assert!(result.is_ok());
        let mut encoder = result.unwrap();
        let k = 5;
        let _ = solver.add_clause([solver.get_literal_for_predicate(predicate![domain >= k + 1])]);
        let result = encoder.strengthen_at_most_k(5, &mut solver);
        assert!(result.is_err());
    }
}
