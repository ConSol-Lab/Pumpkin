use itertools::Itertools;

use crate::basic_types::PropagationStatusCP;
use crate::basic_types::PropagatorConflict;
use crate::basic_types::PropositionalConjunction;
use crate::declare_inference_label;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::notifications::OpaqueDomainEvent;
use crate::engine::propagation::constructor::PropagatorConstructor;
use crate::engine::propagation::constructor::PropagatorConstructorContext;
use crate::engine::propagation::contexts::ManipulateTrailedValues;
use crate::engine::propagation::contexts::PropagationContextWithTrailedValues;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::ExplanationContext;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::variables::IntegerVariable;
use crate::engine::DomainEvents;
use crate::engine::TrailedInteger;
use crate::predicate;
use crate::predicates::Predicate;
use crate::proof::ConstraintTag;
use crate::proof::InferenceCode;
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
    /// Find an irreducible set of atomics such that it is inconsistent w.r.t. the linear constraint
    /// and all atomics of the form [var >= lb] for (var, lb) in the `lower_bounds` list.
    fn extend_reason<Context: ReadDomains>(
        x: &[Var],
        c: i32,
        reason_buffer: &mut Vec<Predicate>,
        context: Context,
        lower_bounds: &[(usize, i32)],
        trail_position: usize,
    ) {
        // Rewrite the constraint x1 + ... + xn >= c with arbitrary lower bounds
        // as (y1 + l1) + ... + (yn + ln) >= c with lk as the global lower bound of xk,
        // and all "variables" y1, ..., yn having lower bounds of 0.
        //
        // Rewrite further as y1 + ... + yn >= c - (l1 + ... + ln) and store the new RHS.
        let mut rem = c - x
            .iter()
            .enumerate()
            .map(|(ix, var)| {
                // Take the lower bound from the input if exists or from context otherwise
                lower_bounds
                    .iter()
                    .filter_map(|(lb_ix, lb)| if *lb_ix == ix { Some(*lb) } else { None })
                    .next()
                    .unwrap_or(context.lower_bound_at_trail_position(var, 0))
            })
            .sum::<i32>();

        // Find the smallest subset of variable bounds from {y1, ..., yn} with the total
        // lower bound exceeding the RHS from the previous step; use the fact that
        // [yk >= t] is equivalent to [xk >= t - lk]
        for var in x
            .iter()
            .enumerate()
            .filter_map(|(ix, var)| {
                if lower_bounds.iter().any(|(lb_ix, _)| *lb_ix == ix) {
                    None
                } else {
                    Some(var)
                }
            })
            .sorted_by_cached_key(|&var| {
                context.lower_bound_at_trail_position(var, 0)
                    - context.lower_bound_at_trail_position(var, trail_position)
            })
        {
            // If the condition on the selected set is satisfied, break
            if rem < 0 {
                break;
            }
            // Add the variable accounting for the largest lower bound (in the shifted sense)
            rem -= context.lower_bound_at_trail_position(var, trail_position)
                - context.lower_bound_at_trail_position(var, 0);
            reason_buffer.push(predicate![
                var >= context.lower_bound_at_trail_position(var, trail_position)
            ]);
        }
    }

    fn create_conflict(&self, context: PropagationContext) -> PropagatorConflict {
        // Find a irreducible set of atomics S such that S -> FALSE
        let mut reason = Vec::new();
        Self::extend_reason(
            &self.x,
            self.c,
            &mut reason,
            context,
            &[],
            context.assignments.num_trail_entries(),
        );
        PropagatorConflict {
            conjunction: reason.clone().into(),
            inference_code: self.inference_code,
        }
    }

    fn explain_propagation(&mut self, context: ExplanationContext, var_index: usize, ub: i32) {
        // Find a irreducible set of atomics S such that S -> [self.x[i] <= ub] <=> S /\ [self.x[i]
        // >= ub + 1] -> FALSE
        let trail_position = context.get_trail_position();
        Self::extend_reason(
            &self.x,
            self.c,
            &mut self.reason_buffer,
            context,
            &[(var_index, ub + 1)],
            trail_position,
        )
    }
}

impl<Var: 'static> Propagator for LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn detect_inconsistency(
        &self,
        context: PropagationContextWithTrailedValues,
    ) -> Option<PropagatorConflict> {
        if (self.c as i64) < context.value(self.lower_bound_left_hand_side) {
            Some(self.create_conflict(context.as_readonly()))
        } else {
            None
        }
    }

    fn notify(
        &mut self,
        mut context: PropagationContextWithTrailedValues,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let index = local_id.unpack() as usize;
        let x_i = &self.x[index];

        let old_bound = context.value(self.current_bounds[index]);
        let new_bound = context.lower_bound(x_i) as i64;

        pumpkin_assert_simple!(
            old_bound < new_bound,
            "propagator should only be triggered when lower bounds are tightened, old_bound={old_bound}, new_bound={new_bound}"
        );

        context.add_assign(self.lower_bound_left_hand_side, new_bound - old_bound);
        context.assign(self.current_bounds[index], new_bound);

        EnqueueDecision::Enqueue
    }

    fn priority(&self) -> u32 {
        0
    }

    fn name(&self) -> &str {
        "LinearLeq"
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> &[Predicate] {
        let i = code as usize;

        self.reason_buffer.clear();

        let ub =
            context.upper_bound_at_trail_position(&self.x[i], context.get_trail_position() + 1);
        self.explain_propagation(context, i, ub);

        &self.reason_buffer
    }

    fn propagate(&mut self, mut context: PropagationContextMut) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.as_trailed_readonly()) {
            return Err(conflict.into());
        }

        let lower_bound_left_hand_side =
            match TryInto::<i32>::try_into(context.value(self.lower_bound_left_hand_side)) {
                Ok(bound) => bound,
                Err(_) if context.value(self.lower_bound_left_hand_side).is_positive() => {
                    // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                    // overflow (hence the check that the lower-bound on the left-hand side is
                    // positive)
                    //
                    // This means that the lower-bounds of the current variables will always be
                    // higher than the right-hand side (with a maximum value of i32). We thus
                    // return a conflict
                    return Err(self.create_conflict(context.as_readonly()).into());
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

    fn debug_propagate_from_scratch(
        &self,
        mut context: PropagationContextMut,
    ) -> PropagationStatusCP {
        let lower_bound_left_hand_side = self
            .x
            .iter()
            .map(|var| context.lower_bound(var) as i64)
            .sum::<i64>();

        let lower_bound_left_hand_side = match TryInto::<i32>::try_into(lower_bound_left_hand_side)
        {
            Ok(bound) => bound,
            Err(_) if context.value(self.lower_bound_left_hand_side).is_positive() => {
                // We cannot fit the `lower_bound_left_hand_side` into an i32 due to an
                // overflow (hence the check that the lower-bound on the left-hand side is
                // positive)
                //
                // This means that the lower-bounds of the current variables will always be
                // higher than the right-hand side (with a maximum value of i32). We thus
                // return a conflict
                return Err(self.create_conflict(context.as_readonly()).into());
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
