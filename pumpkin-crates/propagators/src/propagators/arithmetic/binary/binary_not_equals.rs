use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;

declare_inference_label!(BinaryNotEquals);

/// The [`PropagatorConstructor`] for the [`BinaryNotEqualsPropagator`].
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsPropagatorArgs<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
    pub constraint_tag: ConstraintTag,
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
pub struct BinaryNotEqualsPropagator<AVar, BVar> {
    a: AVar,
    b: BVar,

    inference_code: InferenceCode,
}

impl<AVar, BVar> Propagator for BinaryNotEqualsPropagator<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn detect_inconsistency(&self, domains: Domains) -> Option<PropagatorConflict> {
        // We first check whether they are both fixed
        if domains.is_fixed(&self.a) && domains.is_fixed(&self.b) {
            let lb_a = domains.lower_bound(&self.a);
            let lb_b = domains.lower_bound(&self.b);

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

    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "BinaryNotEq"
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.domains()) {
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

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.domains()) {
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

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use pumpkin_core::TestSolver;
    use pumpkin_core::propagation::EnqueueDecision;

    use crate::propagators::arithmetic::BinaryNotEqualsPropagatorArgs;

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
