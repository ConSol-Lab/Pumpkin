#![allow(clippy::double_parens, reason = "originates inside the bitfield macro")]

use std::slice;

use bitfield_struct::bitfield;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::conjunction;
use crate::containers::HashSet;
use crate::declare_inference_label;
use crate::engine::EmptyDomainConflict;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::predicates::Predicate;
use crate::predicates::PredicateConstructor;
use crate::predicates::PredicateType;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::DomainEvents;
use crate::propagation::EnqueueDecision;
use crate::propagation::ExplanationContext;
use crate::propagation::LocalId;
use crate::propagation::PropagationContext;
use crate::propagation::PropagationContextMut;
use crate::propagation::PropagationContextWithTrailedValues;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_advanced;

declare_inference_label!(BinaryEquals);

/// The [`PropagatorConstructor`] for the [`BinaryEqualsPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct BinaryEqualsPropagatorArgs<AVar, BVar> {
    pub(crate) a: AVar,
    pub(crate) b: BVar,
    pub(crate) constraint_tag: ConstraintTag,
}

impl<AVar, BVar> PropagatorConstructor for BinaryEqualsPropagatorArgs<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    type PropagatorImpl = BinaryEqualsPropagator<AVar, BVar>;

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

            inference_code: context.create_inference_code(constraint_tag, BinaryEquals),

            has_backtracked: false,
            first_propagation_loop: true,
            reason: Predicate::trivially_false(),
        }
    }
}

/// Propagator for the constraint `a = b`.
#[derive(Clone, Debug)]
pub(crate) struct BinaryEqualsPropagator<AVar, BVar> {
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
        context: &mut PropagationContextMut,
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
            self.inference_code,
        )
    }
}

impl<AVar, BVar> Propagator for BinaryEqualsPropagator<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn detect_inconsistency(
        &self,
        context: PropagationContextWithTrailedValues,
    ) -> Option<PropagatorConflict> {
        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        if a_ub < b_lb {
            // If `a` is fully before `b` then we report a conflict
            //
            // Note that we lift the conflict
            Some(PropagatorConflict {
                conjunction: conjunction!([self.a <= b_lb - 1] & [self.b >= b_lb]),
                inference_code: self.inference_code,
            })
        } else if b_ub < a_lb {
            // If `b` is fully before `a` then we report a conflict
            //
            // Note that we lift the conflict
            Some(PropagatorConflict {
                conjunction: conjunction!([self.b <= a_lb - 1] & [self.a >= a_lb]),
                inference_code: self.inference_code,
            })
        } else {
            None
        }
    }

    fn notify(
        &mut self,
        context: PropagationContextWithTrailedValues,
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

    fn synchronise(&mut self, _context: PropagationContext) {
        // Recall that we need to ensure that the stored removed values could now be inaccurate
        self.has_backtracked = true;
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "BinaryEq"
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        if self.first_propagation_loop {
            // If it is the first propagation loop then we do full propagation
            self.first_propagation_loop = false;
            return self.debug_propagate_from_scratch(context);
        }

        if let Some(conflict) = self.detect_inconsistency(context.as_trailed_readonly()) {
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
            self.a_removed_values
                .retain(|element| context.is_predicate_satisfied(predicate!(self.a != *element)));
            self.b_removed_values
                .retain(|element| context.is_predicate_satisfied(predicate!(self.b != *element)));
        }

        // Then we remove all of the values which have been removed from `a` from `b`
        let mut a_removed_values = std::mem::take(&mut self.a_removed_values);
        for removed_value_a in a_removed_values.drain() {
            pumpkin_assert_advanced!(
                context.is_predicate_satisfied(predicate!(self.a != removed_value_a))
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
                context.is_predicate_satisfied(predicate!(self.b != removed_value_b))
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

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
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

#[cfg(test)]
mod tests {
    use crate::engine::test_solver::TestSolver;
    use crate::propagation::EnqueueDecision;
    use crate::propagators::binary::BinaryEqualsPropagatorArgs;

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
