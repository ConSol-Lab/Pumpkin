use pumpkin_core::asserts::pumpkin_assert_simple;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::TrailedInteger;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;

/// Propagator for the constraint `\sum x_i <= c`.
#[derive(Clone, Debug)]
pub struct LinearLessOrEqualPropagator<Var> {
    pub(super) x: Box<[Var]>,
    pub(super) c: i32,

    /// The lower bound of the sum of the left-hand side. This is incremental state.
    pub(super) lower_bound_left_hand_side: TrailedInteger,
    /// The value at index `i` is the bound for `x[i]`.
    pub(super) current_bounds: Box<[TrailedInteger]>,
    /// A buffer for storing the reason for a propagation.
    pub(super) reason_buffer: Vec<Predicate>,

    pub(super) inference_code: InferenceCode,
}

impl<Var> LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn create_conflict(&self, context: Domains) -> PropagatorConflict {
        PropagatorConflict {
            conjunction: self
                .x
                .iter()
                .map(|var| predicate![var >= context.lower_bound(var)])
                .collect(),
            inference_code: self.inference_code.clone(),
        }
    }
}

impl<Var: 'static> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn detect_inconsistency(&self, domains: Domains) -> Option<PropagatorConflict> {
        if (self.c as i64) < domains.read_trailed_integer(self.lower_bound_left_hand_side) {
            Some(self.create_conflict(domains))
        } else {
            None
        }
    }

    fn notify(
        &mut self,
        mut context: NotificationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let index = local_id.unpack() as usize;
        let x_i = &self.x[index];

        let old_bound = context.read_trailed_integer(self.current_bounds[index]);
        let new_bound = context.lower_bound(x_i) as i64;

        pumpkin_assert_simple!(
            old_bound < new_bound,
            "propagator should only be triggered when lower bounds are tightened, old_bound={old_bound}, new_bound={new_bound}"
        );

        context.write_trailed_integer(
            self.lower_bound_left_hand_side,
            context.read_trailed_integer(self.lower_bound_left_hand_side) + (new_bound - old_bound),
        );
        context.write_trailed_integer(self.current_bounds[index], new_bound);

        EnqueueDecision::Enqueue
    }

    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> LazyExplanation<'_> {
        let i = code as usize;

        self.reason_buffer.clear();

        self.reason_buffer
            .extend(self.x.iter().enumerate().filter_map(|(j, x_j)| {
                if j != i {
                    Some(predicate![
                        x_j >= context
                            .lower_bound_at_trail_position(x_j, context.get_trail_position())
                    ])
                } else {
                    None
                }
            }));

        LazyExplanation {
            predicates: self.reason_buffer.as_slice(),
            inference_code: self.inference_code.clone(),
        }
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.domains()) {
            return Err(conflict.into());
        }

        let lower_bound_left_hand_side = match TryInto::<i32>::try_into(
            context.read_trailed_integer(self.lower_bound_left_hand_side),
        ) {
            Ok(bound) => bound,
            Err(_)
                if context
                    .read_trailed_integer(self.lower_bound_left_hand_side)
                    .is_positive() =>
            {
                // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                // overflow (hence the check that the lower-bound on the left-hand side is
                // positive)
                //
                // This means that the lower-bounds of the current variables will always be
                // higher than the right-hand side (with a maximum value of i32). We thus
                // return a conflict
                return Err(self.create_conflict(context.domains()).into());
            }
            Err(_) => {
                // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                // underflow
                //
                // This means that the constraint is always satisfied
                return Ok(());
            }
        };

        for (i, x_i) in self.x.iter().enumerate() {
            let bound = self.c - (lower_bound_left_hand_side - context.lower_bound(x_i));

            if context.upper_bound(x_i) > bound {
                context.post(predicate![x_i <= bound], i)?;
            }
        }

        Ok(())
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        let lower_bound_left_hand_side = self
            .x
            .iter()
            .map(|var| context.lower_bound(var) as i64)
            .sum::<i64>();

        let lower_bound_left_hand_side = match TryInto::<i32>::try_into(lower_bound_left_hand_side)
        {
            Ok(bound) => bound,
            Err(_)
                if context
                    .read_trailed_integer(self.lower_bound_left_hand_side)
                    .is_positive() =>
            {
                // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                // overflow (hence the check that the lower-bound on the left-hand side is
                // positive)
                //
                // This means that the lower-bounds of the current variables will always be
                // higher than the right-hand side (with a maximum value of i32). We thus
                // return a conflict
                return Err(self.create_conflict(context.domains()).into());
            }
            Err(_) => {
                // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                // underflow
                //
                // This means that the constraint is always satisfied
                return Ok(());
            }
        };

        for (i, x_i) in self.x.iter().enumerate() {
            let bound = self.c - (lower_bound_left_hand_side - context.lower_bound(x_i));

            if context.upper_bound(x_i) > bound {
                let reason: PropositionalConjunction = self
                    .x
                    .iter()
                    .enumerate()
                    .filter_map(|(j, x_j)| {
                        if j != i {
                            Some(predicate![x_j >= context.lower_bound(x_j)])
                        } else {
                            None
                        }
                    })
                    .collect();

                context.post(predicate![x_i <= bound], (reason, &self.inference_code))?;
            }
        }

        Ok(())
    }
}
