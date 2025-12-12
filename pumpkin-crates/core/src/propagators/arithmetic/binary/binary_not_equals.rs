use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::conjunction;
use crate::declare_inference_label;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::DomainEvents;
use crate::propagation::LocalId;
use crate::propagation::PropagationContextMut;
use crate::propagation::PropagationContextWithTrailedValues;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;

declare_inference_label!(BinaryNotEquals);

/// The [`PropagatorConstructor`] for the [`BinaryNotEqualsPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct BinaryNotEqualsPropagatorArgs<AVar, BVar> {
    pub(crate) a: AVar,
    pub(crate) b: BVar,
    pub(crate) constraint_tag: ConstraintTag,
}

impl<AVar, BVar> PropagatorConstructor for BinaryNotEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryNotEqualsPropagator<AVar, BVar>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        } = self;

        // We only care about the case where one of the two is assigned
        context.register(a.clone(), DomainEvents::ASSIGN, LocalId::from(0));
        context.register(b.clone(), DomainEvents::ASSIGN, LocalId::from(1));

        BinaryNotEqualsPropagator {
            a,
            b,

            inference_code: context.create_inference_code(constraint_tag, BinaryNotEquals),
        }
    }
}

/// Propagator for the constraint `a != b`.
#[derive(Clone, Debug)]
pub(crate) struct BinaryNotEqualsPropagator<AVar, BVar> {
    a: AVar,
    b: BVar,

    inference_code: InferenceCode,
}

impl<AVar, BVar> Propagator for BinaryNotEqualsPropagator<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn detect_inconsistency(
        &self,
        context: PropagationContextWithTrailedValues,
    ) -> Option<PropagatorConflict> {
        // We first check whether they are both fixed
        if context.is_fixed(&self.a) && context.is_fixed(&self.b) {
            let lb_a = context.lower_bound(&self.a);
            let lb_b = context.lower_bound(&self.b);

            // If they are, then we check whether they are assigned to the same value
            if lb_a == lb_b {
                // If this is the case then we have detected a conflict
                Some(PropagatorConflict {
                    conjunction: conjunction!([self.a == lb_a] & [self.b == lb_a]),
                    inference_code: self.inference_code,
                })
            } else {
                None
            }
        } else {
            None
        }
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "BinaryNotEq"
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.as_trailed_readonly()) {
            return Err(conflict.into());
        }

        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        if a_ub < b_lb || b_ub < a_lb {
            // The domains are non-overlapping
            return Ok(());
        }

        // If `a` is fixed then we can propagate
        if a_lb == a_ub {
            context.post(
                predicate!(self.b != a_lb),
                conjunction!([self.a == a_lb]),
                self.inference_code,
            )?;
        }

        // If `b` is fixed then we can propagate
        if b_lb == b_ub {
            context.post(
                predicate!(self.a != b_lb),
                conjunction!([self.b == b_lb]),
                self.inference_code,
            )?;
        }

        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.as_trailed_readonly()) {
            return Err(conflict.into());
        }

        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        if a_ub < b_lb || b_ub < a_lb {
            return Ok(());
        }

        if a_lb == a_ub {
            context.post(
                predicate!(self.b != a_lb),
                conjunction!([self.a == a_lb]),
                self.inference_code,
            )?;
        }

        if b_lb == b_ub {
            context.post(
                predicate!(self.a != b_lb),
                conjunction!([self.b == b_lb]),
                self.inference_code,
            )?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::test_solver::TestSolver;
    use crate::propagation::EnqueueDecision;
    use crate::propagators::binary::BinaryNotEqualsPropagatorArgs;

    #[test]
    fn detects_conflict() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 0);
        let b = solver.new_variable(0, 0);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(BinaryNotEqualsPropagatorArgs {
                a,
                b,
                constraint_tag,
            })
            .expect_err("Expected conflict to be detected");
    }

    #[test]
    fn propagate_when_one_is_fixed() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 0);
        let b = solver.new_variable(0, 1);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(BinaryNotEqualsPropagatorArgs {
                a,
                b,
                constraint_tag,
            })
            .expect("Expected no conflict to be detected");

        solver.assert_bounds(b, 1, 1);
    }

    #[test]
    fn incremental_propagation() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 0);
        let b = solver.new_variable(0, 10);
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(BinaryNotEqualsPropagatorArgs {
                a,
                b,
                constraint_tag,
            })
            .expect("Expected no conflict to be detected");

        solver.assert_bounds(b, 1, 10);

        solver.new_checkpoint();

        let should_enqueue = solver.decrease_upper_bound_and_notify(propagator, 1, b, 5);
        assert_eq!(should_enqueue, EnqueueDecision::Skip);

        solver.synchronise(0);
        let should_enqueue = solver.decrease_upper_bound_and_notify(propagator, 1, b, 1);
        assert_eq!(should_enqueue, EnqueueDecision::Enqueue);
    }

    #[test]
    fn non_overlapping_is_ok() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 5);
        let b = solver.new_variable(6, 10);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(BinaryNotEqualsPropagatorArgs {
                a,
                b,
                constraint_tag,
            })
            .expect("Expected no conflict to be detected");

        solver.assert_bounds(a, 0, 5);
        solver.assert_bounds(b, 6, 10);
    }
}
