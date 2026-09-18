use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_core::checkers::RetentionChecker;
use pumpkin_core::checkers::Scope;
use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::state::PropagationStatusCP;
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

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        } = self;

        // We only care about the case where one of the two is assigned
        let registration = EventsToRegister::builder()
            .add(&a, DomainEvents::ASSIGN, LocalId::from(0))
            .add(&b, DomainEvents::ASSIGN, LocalId::from(1))
            .build();

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_rule(
            ((LocalId::from(0), &a), (LocalId::from(1), &b)),
            constraint_tag,
            BinaryNotEquals,
            BinaryNotEqualsChecker {
                lhs: a.clone(),
                rhs: b.clone(),
            },
        );

        let propagator = BinaryNotEqualsPropagator {
            a,
            b,

            inference_code,
        };

        PropagatorSpec {
            registration,
            checkers: checkers.build(),
            propagator,
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
        if let Some(fixed_a) = domains.fixed_value(&self.a)
            && let Some(fixed_b) = domains.fixed_value(&self.b)
            && fixed_a == fixed_b
        {
            // If they are, and they are assigned to the same value, then we have detected a
            // conflict
            Some(PropagatorConflict {
                conjunction: conjunction!([self.a == fixed_a] & [self.b == fixed_a]),
                inference_code: self.inference_code.clone(),
            })
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
                (conjunction!([self.a == a_lb]), &self.inference_code),
            )?;
        }

        // If `b` is fixed then we can propagate
        if b_lb == b_ub {
            context.post(
                predicate!(self.a != b_lb),
                (conjunction!([self.b == b_lb]), &self.inference_code),
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
                (conjunction!([self.a == a_lb]), &self.inference_code),
            )?;
        }

        if b_lb == b_ub {
            context.post(
                predicate!(self.a != b_lb),
                (conjunction!([self.b == b_lb]), &self.inference_code),
            )?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct BinaryNotEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}

impl<Lhs, Rhs, Atomic> InferenceChecker<Atomic> for BinaryNotEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // There is a conflict if both variables are fixed to the same values.

        self.lhs.induced_fixed_value(&state) == self.rhs.induced_fixed_value(&state)
    }
}

impl<Lhs, Rhs> RetentionChecker for BinaryNotEqualsChecker<Lhs, Rhs>
where
    Lhs: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    fn check_retention(&mut self, _: &Scope, domains: Domains<'_>) -> bool {
        match (
            domains.fixed_value(&self.lhs),
            domains.fixed_value(&self.rhs),
        ) {
            (Some(lhs), Some(rhs)) => {
                if lhs == rhs {
                    log::error!(
                        "{:?} and {:?} are both fixed to {lhs}; the disequality is violated",
                        self.lhs,
                        self.rhs
                    );
                }
                lhs != rhs
            }
            (Some(value), None) => {
                let is_removed = !domains.contains(&self.rhs, value);
                if !is_removed {
                    log::error!(
                        "The value {value} could be removed from {:?} since {:?} is fixed to it",
                        self.rhs,
                        self.lhs
                    );
                }
                is_removed
            }
            (None, Some(value)) => {
                let is_removed = !domains.contains(&self.lhs, value);
                if !is_removed {
                    log::error!(
                        "The value {value} could be removed from {:?} since {:?} is fixed to it",
                        self.lhs,
                        self.rhs
                    );
                }
                is_removed
            }
            (None, None) => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::state::State;

    use crate::StateExt;
    use crate::propagators::arithmetic::BinaryNotEqualsPropagatorArgs;

    #[test]
    fn detects_conflict() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 0, None);
        let b = state.new_interval_variable(0, 0, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        });
        let _ = state
            .propagate_to_fixed_point()
            .expect_err("Expected conflict to be detected");
    }

    #[test]
    fn propagate_when_one_is_fixed() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 0, None);
        let b = state.new_interval_variable(0, 1, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        });
        state
            .propagate_to_fixed_point()
            .expect("Expected no conflict to be detected");

        state.assert_bounds(b, 1, 1);
    }

    #[allow(deprecated, reason = "Uses TestSolver for EnqueueDecision assertions")]
    #[test]
    fn incremental_propagation() {
        use pumpkin_core::TestSolver;
        use pumpkin_core::propagation::EnqueueDecision;

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
        let mut state = State::default();
        let a = state.new_interval_variable(0, 5, None);
        let b = state.new_interval_variable(6, 10, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        });
        state
            .propagate_to_fixed_point()
            .expect("Expected no conflict to be detected");

        state.assert_bounds(a, 0, 5);
        state.assert_bounds(b, 6, 10);
    }
}

#[cfg(test)]
mod retention_tests {
    use pumpkin_core::state::State;

    use super::*;

    #[test]
    fn retention_fails_when_the_fixed_value_is_present_in_the_other_domain() {
        let mut state = State::default();
        let a = state.new_interval_variable(3, 3, None);
        let b = state.new_interval_variable(0, 5, None);

        let mut checker = BinaryNotEqualsChecker { lhs: a, rhs: b };
        let scope = Scope::from_variables([a, b].iter());

        assert!(!checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_holds_with_both_sides_unfixed() {
        let mut state = State::default();
        let a = state.new_interval_variable(0, 5, None);
        let b = state.new_interval_variable(0, 5, None);

        let mut checker = BinaryNotEqualsChecker { lhs: a, rhs: b };
        let scope = Scope::from_variables([a, b].iter());

        assert!(checker.check_retention(&scope, state.get_domains()));
    }

    #[test]
    fn retention_holds_at_the_fixpoint_of_the_propagator() {
        let mut state = State::default();
        let a = state.new_interval_variable(3, 3, None);
        let b = state.new_interval_variable(0, 5, None);
        let constraint_tag = state.new_constraint_tag();

        let _ = state.add_propagator(BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        });
        state.propagate_to_fixed_point().expect("no empty domains");

        let mut checker = BinaryNotEqualsChecker { lhs: a, rhs: b };
        let scope = Scope::from_variables([a, b].iter());

        assert!(checker.check_retention(&scope, state.get_domains()));
    }
}
