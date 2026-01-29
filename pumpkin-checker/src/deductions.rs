use std::collections::BTreeMap;
use std::rc::Rc;

use drcp_format::ConstraintId;
use drcp_format::IntAtomic;

use crate::inferences::Fact;
use crate::model::Nogood;

/// An inference that was ignored when checking a deduction.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IgnoredInference {
    /// The ID of the ignored inference.
    pub constraint_id: ConstraintId,

    /// The premises that were not satisfied when the inference was evaluated.
    pub unsatisfied_premises: Vec<IntAtomic<String, i32>>,
}

/// A deduction is rejected by the checker.
#[derive(thiserror::Error, Debug, PartialEq, Eq)]
#[error("invalid deduction")]
pub enum InvalidDeduction {
    /// The constraint ID of the deduction is already used by an existing constraint.
    #[error("constraint id {0} already in use")]
    DuplicateConstraintId(ConstraintId),

    /// An inference in the deduction sequence does not exist in the proof stage.
    #[error("inference {0} does not exist")]
    UnknownInference(ConstraintId),

    /// The inferences in the proof stage do not derive an empty domain or an explicit
    /// conflict.
    #[error("no conflict was derived after applying all inferences")]
    NoConflict(Vec<IgnoredInference>),

    /// The premise contains mutually exclusive atomic constraints.
    #[error("the deduction contains inconsistent premises")]
    InconsistentPremises,
}

/// Verify that a deduction is valid given the inferences in the proof stage.
pub fn verify_deduction(
    _deduction: &drcp_format::Deduction<Rc<str>, i32>,
    _facts_in_proof_stage: &BTreeMap<ConstraintId, Fact>,
) -> Result<Nogood, InvalidDeduction> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::atomic;
    use crate::fact;
    use crate::test_utils::constraint_id;
    use crate::test_utils::deduction;

    macro_rules! facts {
        {$($k: expr => $v: expr),* $(,)?} => {
            ::std::collections::BTreeMap::from([$(($crate::test_utils::constraint_id($k), $v),)*])
        };
    }

    #[test]
    fn a_sequence_is_correctly_traversed() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [1, 2, 3]);

        let facts_in_proof_stage = facts! {
            1 => fact!([x >= 5] -> [y <= 4]),
            2 => fact!([y <= 7] -> [z != 10]),
            3 => fact!([y <= 5] & [z != 10] -> [x <= 4]),
        };

        let nogood = verify_deduction(&deduction, &facts_in_proof_stage).expect("valid deduction");
        assert_eq!(Nogood::from(premises), nogood);
    }

    #[test]
    fn an_inference_implying_false_is_a_valid_stopping_condition() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [1, 2, 3]);

        let facts_in_proof_stage = facts! {
            1 => fact!([x >= 5] -> [y <= 4]),
            2 => fact!([y <= 7] -> [z != 10]),
            3 => fact!([y <= 5] & [z != 10] -> false),
        };

        let nogood = verify_deduction(&deduction, &facts_in_proof_stage).expect("valid deduction");
        assert_eq!(Nogood::from(premises), nogood);
    }

    #[test]
    fn inference_order_does_not_need_to_be_by_constraint_id() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [2, 1, 4]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x >= 5] -> [y <= 4]),
            1 => fact!([y <= 7] -> [z != 10]),
            4 => fact!([y <= 5] & [z != 10] -> false),
        };

        let nogood = verify_deduction(&deduction, &facts_in_proof_stage).expect("valid deduction");
        assert_eq!(Nogood::from(premises), nogood);
    }

    #[test]
    fn inconsistent_premises_are_identified() {
        let premises = vec![atomic!([x >= 5]), atomic!([x <= 4])];
        let deduction = deduction(5, premises.clone(), [2]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x == 5] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("inconsistent premises");
        assert_eq!(InvalidDeduction::InconsistentPremises, error);
    }

    #[test]
    fn all_inferences_in_sequence_must_be_in_fact_database() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [1, 2]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x == 5] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("unknown inference");
        assert_eq!(InvalidDeduction::UnknownInference(constraint_id(1)), error);
    }

    #[test]
    fn sequence_that_does_not_terminate_in_conflict_is_rejected() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [2, 1]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x >= 5] -> [y <= 4]),
            1 => fact!([y <= 7] -> [z != 10]),
            4 => fact!([y <= 5] & [z != 10] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("unknown inference");
        assert_eq!(InvalidDeduction::NoConflict(vec![]), error);
    }

    #[test]
    fn inferences_with_unsatisfied_premises_are_ignored() {
        let premises = vec![atomic!([x >= 5])];
        let deduction = deduction(5, premises.clone(), [2, 1]);

        let facts_in_proof_stage = facts! {
            2 => fact!([x >= 5] -> [y <= 4]),
            1 => fact!([y <= 7] & [x >= 6] -> [z != 10]),
            4 => fact!([y <= 5] & [z != 10] -> false),
        };

        let error =
            verify_deduction(&deduction, &facts_in_proof_stage).expect_err("unknown inference");
        assert_eq!(
            InvalidDeduction::NoConflict(vec![IgnoredInference {
                constraint_id: constraint_id(1),
                unsatisfied_premises: vec![atomic!([x string >= 6])],
            }]),
            error
        );
    }
}
