use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::basic_types::PropositionalConjunction;
use crate::declare_inference_label;
use crate::engine::TrailedInteger;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::variables::IntegerVariable;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
use crate::propagation::DomainEvents;
use crate::propagation::Domains;
use crate::propagation::EnqueueDecision;
use crate::propagation::ExplanationContext;
use crate::propagation::LocalId;
use crate::propagation::NotificationContext;
use crate::propagation::Priority;
use crate::propagation::PropagationContext;
use crate::propagation::Propagator;
use crate::propagation::PropagatorConstructor;
use crate::propagation::PropagatorConstructorContext;
use crate::propagation::ReadDomains;
use crate::pumpkin_assert_simple;

declare_inference_label!(LinearBounds);

/// The [`PropagatorConstructor`] for the [`LinearLessOrEqualPropagator`].
#[derive(Clone, Debug)]
pub(crate) struct LinearLessOrEqualPropagatorArgs<Var> {
    pub(crate) x: Box<[Var]>,
    pub(crate) c: i32,
    pub(crate) constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for LinearLessOrEqualPropagatorArgs<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = LinearLessOrEqualPropagator<Var>;

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let LinearLessOrEqualPropagatorArgs {
            x,
            c,
            constraint_tag,
        } = self;

        let mut lower_bound_left_hand_side = 0_i64;
        let mut current_bounds = vec![];

        for (i, x_i) in x.iter().enumerate() {
            context.register(
                x_i.clone(),
                DomainEvents::LOWER_BOUND,
                LocalId::from(i as u32),
            );
            lower_bound_left_hand_side += context.lower_bound(x_i) as i64;
            current_bounds.push(context.new_trailed_integer(context.lower_bound(x_i) as i64));
        }

        let lower_bound_left_hand_side = context.new_trailed_integer(lower_bound_left_hand_side);

        LinearLessOrEqualPropagator {
            x,
            c,
            lower_bound_left_hand_side,
            current_bounds: current_bounds.into(),
            inference_code: context.create_inference_code(constraint_tag, LinearBounds),
            reason_buffer: Vec::default(),
        }
    }
}

/// Propagator for the constraint `\sum x_i <= c`.
#[derive(Clone, Debug)]
pub(crate) struct LinearLessOrEqualPropagator<Var> {
    x: Box<[Var]>,
    c: i32,

    /// The lower bound of the sum of the left-hand side. This is incremental state.
    lower_bound_left_hand_side: TrailedInteger,
    /// The value at index `i` is the bound for `x[i]`.
    current_bounds: Box<[TrailedInteger]>,
    /// A buffer for storing the reason for a propagation.
    reason_buffer: Vec<Predicate>,

    inference_code: InferenceCode,
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
            inference_code: self.inference_code,
        }
    }
}

impl<Var: 'static> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn detect_inconsistency(&self, domains: Domains) -> Option<PropagatorConflict> {
        if (self.c as i64) < domains.value_trailed_integer(self.lower_bound_left_hand_side) {
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

        let old_bound = context.value_trailed_integer(self.current_bounds[index]);
        let new_bound = context.lower_bound(x_i) as i64;

        pumpkin_assert_simple!(
            old_bound < new_bound,
            "propagator should only be triggered when lower bounds are tightened, old_bound={old_bound}, new_bound={new_bound}"
        );

        context.add_assign_trailed_integer(self.lower_bound_left_hand_side, new_bound - old_bound);
        context.assign_trailed_integer(self.current_bounds[index], new_bound);

        EnqueueDecision::Enqueue
    }

    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> &[Predicate] {
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

        &self.reason_buffer
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.domains()) {
            return Err(conflict.into());
        }

        let lower_bound_left_hand_side = match TryInto::<i32>::try_into(
            context.value_trailed_integer(self.lower_bound_left_hand_side),
        ) {
            Ok(bound) => bound,
            Err(_)
                if context
                    .value_trailed_integer(self.lower_bound_left_hand_side)
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
                context.post(predicate![x_i <= bound], i, self.inference_code)?;
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
                    .value_trailed_integer(self.lower_bound_left_hand_side)
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

                context.post(predicate![x_i <= bound], reason, self.inference_code)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_solver::TestSolver;

    #[test]
    fn test_bounds_are_propagated() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);

        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(LinearLessOrEqualPropagatorArgs {
                x: [x, y].into(),
                c: 7,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.propagate(propagator).expect("non-empty domain");

        solver.assert_bounds(x, 1, 5);
        solver.assert_bounds(y, 0, 6);
    }

    #[test]
    fn test_explanations() {
        let mut solver = TestSolver::default();
        let x = solver.new_variable(1, 5);
        let y = solver.new_variable(0, 10);
        let constraint_tag = solver.new_constraint_tag();

        let propagator = solver
            .new_propagator(LinearLessOrEqualPropagatorArgs {
                x: [x, y].into(),
                c: 7,
                constraint_tag,
            })
            .expect("no empty domains");

        solver.propagate(propagator).expect("non-empty domain");

        let reason = solver.get_reason_int(predicate![y <= 6]);

        assert_eq!(conjunction!([x >= 1]), reason);
    }

    #[test]
    fn overflow_leads_to_conflict() {
        let mut solver = TestSolver::default();

        let x = solver.new_variable(i32::MAX, i32::MAX);
        let y = solver.new_variable(1, 1);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(LinearLessOrEqualPropagatorArgs {
                x: [x, y].into(),
                c: i32::MAX,
                constraint_tag,
            })
            .expect_err("Expected overflow to be detected");
    }

    #[test]
    fn underflow_leads_to_no_propagation() {
        let mut solver = TestSolver::default();

        let x = solver.new_variable(i32::MIN, i32::MIN);
        let y = solver.new_variable(-1, -1);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(LinearLessOrEqualPropagatorArgs {
                x: [x, y].into(),
                c: i32::MIN,
                constraint_tag,
            })
            .expect("Expected no error to be detected");
    }
}
