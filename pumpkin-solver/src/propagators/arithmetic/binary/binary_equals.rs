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

/// The [`PropagatorConstructor`] for the [`LinearLessOrEqualPropagator`].
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

/// Propagator for the constraint `\sum x_i <= c`.
#[derive(Clone, Debug)]
pub(crate) struct BinaryEqualsPropagator<AVar, BVar> {
    a: AVar,
    b: BVar,

    a_removed_values: HashSet<i32>,
    b_removed_values: HashSet<i32>,

    has_backtracked: bool,
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
            Some(PropagatorConflict {
                conjunction: conjunction!([self.a <= b_lb - 1] & [self.b >= b_lb]),
                inference_code: self.inference_code,
            })
        } else if b_ub < a_lb {
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
                    self.a_removed_values
                        .extend(context.get_holes_on_current_decision_level(&self.a));
                }
            }
            1 => {
                if matches!(self.b.unpack_event(event), DomainEvent::Removal) {
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

        if self.has_backtracked {
            self.has_backtracked = false;
            self.a_removed_values
                .retain(|element| context.is_predicate_satisfied(predicate!(self.a != *element)));
            self.b_removed_values
                .retain(|element| context.is_predicate_satisfied(predicate!(self.b != *element)));
        }

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
        if let Some(conflict) = self.detect_inconsistency(context.as_trailed_readonly()) {
            return Err(conflict.into());
        }

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

        for value_a in context.lower_bound(&self.a)..context.upper_bound(&self.a) {
            if !context.contains(&self.a, value_a) {
                context.post(
                    predicate!(self.b != value_a),
                    conjunction!([self.a != value_a]),
                    self.inference_code,
                )?;
            }
        }

        for value_b in context.lower_bound(&self.b)..context.upper_bound(&self.b) {
            if !context.contains(&self.b, value_b) {
                context.post(
                    predicate!(self.a != value_b),
                    conjunction!([self.b != value_b]),
                    self.inference_code,
                )?;
            }
        }

        Ok(())
    }
}
