use crate::basic_types::HashSet;
use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::conjunction;
use crate::declare_inference_label;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::notifications::DomainEvent;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::variables::IntegerVariable;
use crate::engine::DomainEvents;
use crate::predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
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

        // If we backtrack then we need to update the removable values
        context.register_for_backtrack_events(a.clone(), DomainEvents::REMOVAL, LocalId::from(0));
        context.register_for_backtrack_events(b.clone(), DomainEvents::REMOVAL, LocalId::from(1));

        BinaryEqualsPropagator {
            a,
            b,

            a_removed_values: HashSet::default(),
            b_removed_values: HashSet::default(),

            inference_code: context.create_inference_code(constraint_tag, BinaryEquals),

            has_backtracked: false,
            first_propagation_loop: true,
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
                        .extend(context.get_holes_on_current_decision_level(&self.a));
                }
            }
            1 => {
                if matches!(self.b.unpack_event(event), DomainEvent::Removal) {
                    // If it is a removal then we need to make sure that all of the removed values
                    // from `b` are also removed from `a`
                    self.b_removed_values
                        .extend(context.get_holes_on_current_decision_level(&self.b));
                }
            }
            _ => panic!("Unexpected local id {local_id:?}"),
        }

        EnqueueDecision::Enqueue
    }

    fn notify_backtrack(
        &mut self,
        _context: PropagationContext,
        _local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) {
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
        context.post(
            predicate!(self.a >= b_lb),
            conjunction!([self.b >= b_lb]),
            self.inference_code,
        )?;
        context.post(
            predicate!(self.a <= b_ub),
            conjunction!([self.b <= b_ub]),
            self.inference_code,
        )?;
        context.post(
            predicate!(self.b >= a_lb),
            conjunction!([self.a >= a_lb]),
            self.inference_code,
        )?;
        context.post(
            predicate!(self.b <= a_ub),
            conjunction!([self.a <= a_ub]),
            self.inference_code,
        )?;

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
        for removed_value_a in self.a_removed_values.drain() {
            pumpkin_assert_advanced!(
                context.is_predicate_satisfied(predicate!(self.a != removed_value_a))
            );
            context.post(
                predicate!(self.b != removed_value_a),
                conjunction!([self.a != removed_value_a]),
                self.inference_code,
            )?;
        }

        // Then we remove all of the values which have been removed from `b` from `a`
        for removed_value_b in self.b_removed_values.drain() {
            pumpkin_assert_advanced!(
                context.is_predicate_satisfied(predicate!(self.b != removed_value_b))
            );
            context.post(
                predicate!(self.a != removed_value_b),
                conjunction!([self.b != removed_value_b]),
                self.inference_code,
            )?;
        }

        Ok(())
    }

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        context.post(
            predicate!(self.a >= b_lb),
            conjunction!([self.b >= b_lb]),
            self.inference_code,
        )?;
        context.post(
            predicate!(self.a <= b_ub),
            conjunction!([self.b <= b_ub]),
            self.inference_code,
        )?;
        context.post(
            predicate!(self.b >= a_lb),
            conjunction!([self.a >= a_lb]),
            self.inference_code,
        )?;
        context.post(
            predicate!(self.b <= a_ub),
            conjunction!([self.a <= a_ub]),
            self.inference_code,
        )?;

        for value_a in context.get_holes(&self.a).collect::<Vec<_>>() {
            context.post(
                predicate!(self.b != value_a),
                conjunction!([self.a != value_a]),
                self.inference_code,
            )?;
        }

        for value_b in context.get_holes(&self.b).collect::<Vec<_>>() {
            context.post(
                predicate!(self.a != value_b),
                conjunction!([self.b != value_b]),
                self.inference_code,
            )?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::engine::propagation::EnqueueDecision;
    use crate::engine::test_solver::TestSolver;
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
