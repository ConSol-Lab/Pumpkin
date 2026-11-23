use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::InferenceChecker;
use pumpkin_core::conjunction;
use pumpkin_core::constraints::ReifiablePropagatorConstructor;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::ImpliedBy;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotReified;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorSpec;
use pumpkin_core::propagation::REIFICATION_LOCAL_ID;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::Reification;
use pumpkin_core::propagation::RuntimeCheckers;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;
use pumpkin_core::variables::Literal;

declare_inference_label!(BinaryNotEquals);

/// The [`PropagatorConstructor`] for the [`BinaryNotEqualsPropagator`].
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsPropagatorArgs<AVar, BVar, R = NotReified> {
    pub a: AVar,
    pub b: BVar,
    pub constraint_tag: ConstraintTag,
    pub reification: R,
}

impl<AVar, BVar> ReifiablePropagatorConstructor for BinaryNotEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type ImpliedBy = BinaryNotEqualsPropagatorArgs<AVar, BVar, ImpliedBy>;

    fn into_implied_by(self, reification_literal: Literal) -> Self::ImpliedBy {
        BinaryNotEqualsPropagatorArgs {
            a: self.a,
            b: self.b,
            constraint_tag: self.constraint_tag,
            reification: ImpliedBy(reification_literal),
        }
    }
}

impl<AVar, BVar, R> PropagatorConstructor for BinaryNotEqualsPropagatorArgs<AVar, BVar, R>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
    R: Reification,
{
    type PropagatorImpl = BinaryNotEqualsPropagator<AVar, BVar, R>;

    fn create(self, _: PropagatorConstructorContext) -> PropagatorSpec<Self::PropagatorImpl> {
        let BinaryNotEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
            reification,
        } = self;

        // We only care about the case where one of the two is assigned
        let mut registration = EventsToRegister::builder()
            .add(&a, DomainEvents::ASSIGN, LocalId::from(0))
            .add(&b, DomainEvents::ASSIGN, LocalId::from(1))
            .build();
        reification.register(&mut registration);

        let mut checkers = RuntimeCheckers::builder();
        let inference_code = checkers.add_inference_checker(
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
            reification,
        };

        PropagatorSpec {
            registration,
            checkers: checkers.build().reified(reification),
            propagator,
        }
    }
}

/// Propagator for the constraint `a != b`, or `r -> a != b` if it is half-reified.
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsPropagator<AVar, BVar, R = NotReified> {
    a: AVar,
    b: BVar,

    inference_code: InferenceCode,

    /// Whether the propagator is half-reified.
    reification: R,
}

impl<AVar, BVar, R> BinaryNotEqualsPropagator<AVar, BVar, R>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
    R: Reification,
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
}

impl<AVar, BVar, R> Propagator for BinaryNotEqualsPropagator<AVar, BVar, R>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
    R: Reification,
{
    fn notify(
        &mut self,
        mut context: NotificationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        if local_id == REIFICATION_LOCAL_ID {
            return EnqueueDecision::Enqueue;
        }

        self.reification.filter_enqueue_decision(
            &mut context,
            EnqueueDecision::Enqueue,
            |domains| self.detect_inconsistency(domains),
        )
    }

    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "BinaryNotEq"
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if !self
            .reification
            .should_propagate(&mut context, |domains| self.detect_inconsistency(domains))?
        {
            return Ok(());
        }

        let reification = self.reification;
        reification.add_to_conflict(self.propagate_enforced(context))
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        if !self
            .reification
            .should_propagate(&mut context, |domains| self.detect_inconsistency(domains))?
        {
            return Ok(());
        }

        self.reification
            .add_to_conflict(self.propagate_from_scratch_enforced(context))
    }
}

impl<AVar, BVar, R> BinaryNotEqualsPropagator<AVar, BVar, R>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
    R: Reification,
{
    fn propagate_enforced(&self, mut context: PropagationContext) -> PropagationStatusCP {
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

    fn propagate_from_scratch_enforced(
        &self,
        mut context: PropagationContext,
    ) -> PropagationStatusCP {
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

#[cfg(test)]
mod tests {
    use pumpkin_core::propagation::NotReified;
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
            reification: NotReified,
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
            reification: NotReified,
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
                reification: NotReified,
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
            reification: NotReified,
        });
        state
            .propagate_to_fixed_point()
            .expect("Expected no conflict to be detected");

        state.assert_bounds(a, 0, 5);
        state.assert_bounds(b, 6, 10);
    }
}
