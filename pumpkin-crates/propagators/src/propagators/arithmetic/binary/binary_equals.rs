#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]
use std::slice;

use bitfield_struct::bitfield;
use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::I32Ext;
use pumpkin_checking::InferenceChecker;
use pumpkin_core::asserts::pumpkin_assert_advanced;
use pumpkin_core::conjunction;
use pumpkin_core::containers::HashSet;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PredicateConstructor;
use pumpkin_core::predicates::PredicateType;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvent;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::state::EmptyDomainConflict;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;

declare_inference_label!(BinaryEquals);

/// The [`PropagatorConstructor`] for the [`BinaryEqualsPropagator`].
#[derive(Clone, Debug)]
pub struct BinaryEqualsPropagatorArgs<AVar, BVar> {
    pub a: AVar,
    pub b: BVar,
    pub constraint_tag: ConstraintTag,
}

impl<AVar, BVar> PropagatorConstructor for BinaryEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryEqualsPropagator<AVar, BVar>;

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, BinaryEquals),
            Box::new(BinaryEqualsChecker {
                lhs: self.a.clone(),
                rhs: self.b.clone(),
            }),
        );
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let BinaryEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        } = self;

        context.register(a.clone(), DomainEvents::ANY_INT, LocalId::from(0));
        context.register(b.clone(), DomainEvents::ANY_INT, LocalId::from(1));

        BinaryEqualsPropagator {
            a,
            b,

            a_removed_values: HashSet::default(),
            b_removed_values: HashSet::default(),

            inference_code: InferenceCode::new(constraint_tag, BinaryEquals),

            has_backtracked: false,
            first_propagation_loop: true,
            reason: Predicate::trivially_false(),
        }
    }
}

/// Propagator for the constraint `a = b`.
#[derive(Clone, Debug)]
pub struct BinaryEqualsPropagator<AVar, BVar> {
    a: AVar,
    b: BVar,

    /// The removed value from [`Self::a`].
    ///
    /// These are tracked to make sure that they are also removed from [`Self::b`].
    a_removed_values: HashSet<i32>,
    /// The removed value from [`Self::b`]
    ///
    /// These are tracked to make sure that they are also removed from [`Self::a`].
    b_removed_values: HashSet<i32>,

    /// If a backtrack has occurred which caused one of the removals to be backtracked then we need
    /// to ensure that we do not erroneously remove values which are now part of the domain after
    /// backtracking.
    has_backtracked: bool,

    /// If it is the first time that the propagator is called then we need to ensure that the
    /// domains of [`Self::a`] and [`Self::b`] are equal to the intersection of these domains.
    first_propagation_loop: bool,

    inference_code: InferenceCode,

    /// A re-usable buffer to store the explanations of propagations. This will always be a single
    /// [`Predicate`].
    ///
    /// This field is only written to in the `lazy_explanation` function, as that returns a slice
    /// which needs to be owned somewhere. Hence we put that ownership here.
    reason: Predicate,
}

impl<AVar, BVar> BinaryEqualsPropagator<AVar, BVar>
where
    AVar: PredicateConstructor<Value = i32>,
    BVar: PredicateConstructor<Value = i32>,
{
    fn post(
        &self,
        context: &mut PropagationContext,
        variable: Variable,
        predicate_type: PredicateType,
        value: i32,
    ) -> Result<(), EmptyDomainConflict> {
        use PredicateType::*;
        use Variable::*;

        let predicate = match (variable, predicate_type) {
            (A, LowerBound) => predicate![self.a >= value],
            (A, UpperBound) => predicate![self.a <= value],
            (A, NotEqual) => predicate![self.a != value],
            (A, Equal) => predicate![self.a == value],
            (B, LowerBound) => predicate![self.b >= value],
            (B, UpperBound) => predicate![self.b <= value],
            (B, NotEqual) => predicate![self.b != value],
            (B, Equal) => predicate![self.b == value],
        };

        context.post(
            predicate,
            BinaryEqualsPropagation::new()
                .with_variable(variable)
                .with_predicate_type(predicate_type)
                .with_value(value)
                .into_bits(),
            &self.inference_code,
        )
    }
}

impl<AVar, BVar> Propagator for BinaryEqualsPropagator<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn detect_inconsistency(&self, domains: Domains) -> Option<PropagatorConflict> {
        let a_lb = domains.lower_bound(&self.a);
        let a_ub = domains.upper_bound(&self.a);

        let b_lb = domains.lower_bound(&self.b);
        let b_ub = domains.upper_bound(&self.b);

        if a_ub < b_lb {
            // If `a` is fully before `b` then we report a conflict
            //
            // Note that we lift the conflict
            Some(PropagatorConflict {
                conjunction: conjunction!([self.a <= b_lb - 1] & [self.b >= b_lb]),
                inference_code: self.inference_code.clone(),
            })
        } else if b_ub < a_lb {
            // If `b` is fully before `a` then we report a conflict
            //
            // Note that we lift the conflict
            Some(PropagatorConflict {
                conjunction: conjunction!([self.b <= a_lb - 1] & [self.a >= a_lb]),
                inference_code: self.inference_code.clone(),
            })
        } else {
            None
        }
    }

    fn notify(
        &mut self,
        context: NotificationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        match local_id.unpack() {
            0 => {
                if matches!(self.a.unpack_event(event), DomainEvent::Removal) {
                    // If it is a removal then we need to make sure that all of the removed values
                    // from `a` are also removed from `b`
                    self.a_removed_values
                        .extend(context.get_holes_at_current_checkpoint(&self.a));
                }
            }
            1 => {
                if matches!(self.b.unpack_event(event), DomainEvent::Removal) {
                    // If it is a removal then we need to make sure that all of the removed values
                    // from `b` are also removed from `a`
                    self.b_removed_values
                        .extend(context.get_holes_at_current_checkpoint(&self.b));
                }
            }
            _ => panic!("Unexpected local id {local_id:?}"),
        }

        EnqueueDecision::Enqueue
    }

    fn synchronise(&mut self, _context: Domains) {
        // Recall that we need to ensure that the stored removed values could now be inaccurate
        self.has_backtracked = true;
    }

    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "BinaryEq"
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if self.first_propagation_loop {
            // If it is the first propagation loop then we do full propagation
            self.first_propagation_loop = false;
            return self.propagate_from_scratch(context);
        }

        if let Some(conflict) = self.detect_inconsistency(context.domains()) {
            return Err(conflict.into());
        }

        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        // Now we must ensure that the bounds are equal
        self.post(&mut context, Variable::A, PredicateType::LowerBound, b_lb)?;
        self.post(&mut context, Variable::A, PredicateType::UpperBound, b_ub)?;
        self.post(&mut context, Variable::B, PredicateType::LowerBound, a_lb)?;
        self.post(&mut context, Variable::B, PredicateType::UpperBound, a_ub)?;

        // Now we check whether a backtrack operation has occurred which means that we need to
        // re-evaluate the values which have been removed
        if self.has_backtracked {
            self.has_backtracked = false;
            self.a_removed_values.retain(|element| {
                context.evaluate_predicate(predicate!(self.a != *element)) == Some(true)
            });
            self.b_removed_values.retain(|element| {
                context.evaluate_predicate(predicate!(self.b != *element)) == Some(true)
            });
        }

        // Then we remove all of the values which have been removed from `a` from `b`
        let mut a_removed_values = std::mem::take(&mut self.a_removed_values);
        for removed_value_a in a_removed_values.drain() {
            pumpkin_assert_advanced!(
                context.evaluate_predicate(predicate!(self.a != removed_value_a)) == Some(true)
            );
            self.post(
                &mut context,
                Variable::B,
                PredicateType::NotEqual,
                removed_value_a,
            )?;
        }
        self.a_removed_values = a_removed_values;

        // Then we remove all of the values which have been removed from `b` from `a`
        let mut b_removed_values = std::mem::take(&mut self.b_removed_values);
        for removed_value_b in b_removed_values.drain() {
            pumpkin_assert_advanced!(
                context.evaluate_predicate(predicate!(self.b != removed_value_b)) == Some(true)
            );
            self.post(
                &mut context,
                Variable::A,
                PredicateType::NotEqual,
                removed_value_b,
            )?;
        }

        self.b_removed_values = b_removed_values;

        Ok(())
    }

    fn lazy_explanation(&mut self, code: u64, _: ExplanationContext) -> &[Predicate] {
        use PredicateType::*;
        use Variable::*;

        let propagated = BinaryEqualsPropagation::from_bits(code);

        let explanation = match (propagated.variable(), propagated.predicate_type()) {
            (A, LowerBound) => predicate![self.b >= propagated.value()],
            (A, UpperBound) => predicate![self.b <= propagated.value()],
            (A, NotEqual) => predicate![self.b != propagated.value()],
            (A, Equal) => predicate![self.b == propagated.value()],
            (B, LowerBound) => predicate![self.a >= propagated.value()],
            (B, UpperBound) => predicate![self.a <= propagated.value()],
            (B, NotEqual) => predicate![self.a != propagated.value()],
            (B, Equal) => predicate![self.a == propagated.value()],
        };

        self.reason = explanation;

        slice::from_ref(&self.reason)
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        self.post(&mut context, Variable::A, PredicateType::LowerBound, b_lb)?;
        self.post(&mut context, Variable::A, PredicateType::UpperBound, b_ub)?;
        self.post(&mut context, Variable::B, PredicateType::LowerBound, a_lb)?;
        self.post(&mut context, Variable::B, PredicateType::UpperBound, a_ub)?;

        for removed_value_a in context.get_holes(&self.a).collect::<Vec<_>>() {
            self.post(
                &mut context,
                Variable::B,
                PredicateType::NotEqual,
                removed_value_a,
            )?;
        }

        for removed_value_b in context.get_holes(&self.b).collect::<Vec<_>>() {
            self.post(
                &mut context,
                Variable::A,
                PredicateType::NotEqual,
                removed_value_b,
            )?;
        }

        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
enum Variable {
    A = 0,
    B = 1,
}

impl Variable {
    const fn into_bits(self) -> u8 {
        self as _
    }

    const fn from_bits(value: u8) -> Variable {
        match value {
            0 => Variable::A,
            1 => Variable::B,
            _ => panic!("Unknown bit sequence"),
        }
    }
}

/// Represents the data required for a binary equals propagation.
#[bitfield(u64)]
struct BinaryEqualsPropagation {
    /// The variable for which the propagation takes place.
    #[bits(8)]
    variable: Variable,
    /// The type of propagation (e.g. it could be a [`PredicateType::LowerBound`] propagation).
    #[bits(8)]
    predicate_type: PredicateType,
    /// The value of the propagation
    value: i32,
    /// Padding
    #[bits(16)]
    __: u16,
}

#[derive(Clone, Debug)]
pub struct BinaryEqualsChecker<Lhs, Rhs> {
    pub lhs: Lhs,
    pub rhs: Rhs,
}

impl<Lhs, Rhs, Atomic> InferenceChecker<Atomic> for BinaryEqualsChecker<Lhs, Rhs>
where
    Atomic: AtomicConstraint,
    Lhs: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        mut state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        // We apply the domain of variable 2 to variable 1. If the state remains consistent, then
        // the step is unsound!
        let mut consistent = true;

        if let I32Ext::I32(value) = self.rhs.induced_upper_bound(&state) {
            let atomic = self.lhs.atomic_less_than(value);
            consistent &= state.apply(&atomic);
        }

        if let I32Ext::I32(value) = self.rhs.induced_lower_bound(&state) {
            let atomic = self.lhs.atomic_greater_than(value);
            consistent &= state.apply(&atomic);
        }

        for value in self.rhs.induced_holes(&state).collect::<Vec<_>>() {
            let atomic = self.lhs.atomic_not_equal(value);
            consistent &= state.apply(&atomic);
        }

        !consistent
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use pumpkin_core::TestSolver;
    use pumpkin_core::propagation::EnqueueDecision;

    use crate::propagators::arithmetic::BinaryEqualsPropagatorArgs;

    #[test]
    fn test_propagation_of_bounds() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 5);
        let b = solver.new_variable(3, 7);
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(BinaryEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        });

        assert!(result.is_ok());

        solver.assert_bounds(a, 3, 5);
        solver.assert_bounds(b, 3, 5);
    }

    #[test]
    fn test_propagation_of_holes() {
        let mut solver = TestSolver::default();
        let a = solver.new_sparse_variable(vec![2, 4, 6, 9]);
        let b = solver.new_sparse_variable(vec![3, 4, 7, 9]);
        let constraint_tag = solver.new_constraint_tag();

        let result = solver.new_propagator(BinaryEqualsPropagatorArgs {
            a,
            b,
            constraint_tag,
        });

        assert!(result.is_ok());

        solver.assert_bounds(a, 4, 9);
        solver.assert_bounds(b, 4, 9);

        for i in 5..=8 {
            assert!(!solver.contains(a, i));
            assert!(!solver.contains(b, i));
        }
    }

    #[test]
    fn test_propagation_of_holes_incremental() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(2, 9);
        let b = solver.new_variable(3, 9);
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(BinaryEqualsPropagatorArgs {
                a,
                b,
                constraint_tag,
            })
            .expect("Expected result to be okay");

        solver.assert_bounds(a, 3, 9);
        solver.assert_bounds(b, 3, 9);

        let should_enqueue = solver.remove_and_notify(propagator, a, 5);
        assert_eq!(should_enqueue, EnqueueDecision::Enqueue);

        let should_enqueue = solver.remove_and_notify(propagator, a, 6);
        assert_eq!(should_enqueue, EnqueueDecision::Enqueue);

        let should_enqueue = solver.remove_and_notify(propagator, b, 4);
        assert_eq!(should_enqueue, EnqueueDecision::Enqueue);

        let result = solver.propagate(propagator);
        assert!(result.is_ok());

        assert!(!solver.contains(b, 5));
        assert!(!solver.contains(b, 6));
        assert!(!solver.contains(a, 4));
    }

    #[test]
    fn test_conflict() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(0, 5);
        let b = solver.new_variable(6, 9);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(BinaryEqualsPropagatorArgs {
                a,
                b,
                constraint_tag,
            })
            .expect_err("Expected result to be err");
    }
}
