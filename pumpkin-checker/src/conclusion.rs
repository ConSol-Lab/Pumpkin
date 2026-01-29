use std::rc::Rc;

use crate::model::Model;

pub fn verify_conclusion(
    _model: &Model,
    _conclusion: &drcp_format::Conclusion<Rc<str>, i32>,
) -> bool {
    todo!()
}

#[cfg(test)]
mod tests {
    use drcp_format::Conclusion::*;

    use super::*;
    use crate::atomic;
    use crate::model::Atomic;
    use crate::model::Nogood;
    use crate::test_utils::constraint_id;

    #[test]
    fn unsat_is_accepted_if_model_contains_true_implies_false() {
        let mut model = Model::default();

        #[allow(trivial_casts, reason = "otherwise we need a temporary variable")]
        let _ = model.add_constraint(constraint_id(1), Nogood::from([] as [Atomic; _]));

        assert!(verify_conclusion(&model, &Unsat));
    }

    #[test]
    fn unsat_is_rejected_if_model_does_not_contain_true_implies_false() {
        let model = Model::default();

        assert!(!verify_conclusion(&model, &Unsat));
    }

    #[test]
    fn dual_bound_is_accepted_if_bound_is_shown_by_nogood() {
        let mut model = Model::default();
        model.objective = Some(crate::model::Objective::Maximize(
            fzn_rs::VariableExpr::Identifier("x".into()).into(),
        ));

        let _ = model.add_constraint(constraint_id(1), Nogood::from([atomic!([x >= 5])]));

        assert!(verify_conclusion(&model, &DualBound(atomic!([x <= 4]))));
    }

    #[test]
    fn stronger_claim_is_not_accepted_for_dual_bound() {
        let mut model = Model::default();
        model.objective = Some(crate::model::Objective::Maximize(
            fzn_rs::VariableExpr::Identifier("x".into()).into(),
        ));

        let _ = model.add_constraint(constraint_id(1), Nogood::from([atomic!([x >= 2])]));

        assert!(!verify_conclusion(&model, &DualBound(atomic!([x <= 4]))));
    }

    #[test]
    fn dual_bound_is_only_accepted_if_model_is_optimization() {
        let mut model = Model::default();

        let _ = model.add_constraint(constraint_id(1), Nogood::from([atomic!([x >= 5])]));

        assert!(!verify_conclusion(&model, &DualBound(atomic!([x <= 4]))));
    }

    #[test]
    fn unsat_is_also_accepted_if_there_is_an_objective() {
        let mut model = Model::default();
        model.objective = Some(crate::model::Objective::Maximize(
            fzn_rs::VariableExpr::Identifier("x".into()).into(),
        ));

        #[allow(trivial_casts, reason = "otherwise we need a temporary variable")]
        let _ = model.add_constraint(constraint_id(1), Nogood::from([] as [Atomic; _]));

        assert!(!verify_conclusion(&model, &Unsat));
    }
}
