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
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::variables::IntegerVariable;
use crate::engine::DomainEvents;
use crate::engine::TrailedInteger;
use crate::predicate;
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
            mut x,
            mut c,
            constraint_tag,
        } = self;

        c -= x
            .iter()
            .filter(|var| context.lower_bound(*var) == context.upper_bound(*var))
            .map(|var| context.lower_bound(var))
            .sum::<i32>();
        x = x
            .iter()
            .filter(|var| context.lower_bound(*var) != context.upper_bound(*var))
            .cloned()
            .collect();

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

    inference_code: InferenceCode,
}

impl<Var> LinearLessOrEqualPropagator<Var>
where
    Var: IntegerVariable,
{
    fn create_conflict(&self, context: PropagationContext) -> PropagatorConflict {
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

        let x = solver.new_variable(0, i32::MAX);
        let y = solver.new_variable(1, 2);
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

        let x = solver.new_variable(i32::MIN, 0);
        let y = solver.new_variable(-2, -1);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(LinearLessOrEqualPropagatorArgs {
                x: [x, y].into(),
                c: i32::MIN,
                constraint_tag,
            })
            .expect("Expected no error to be detected");
    }

    #[test]
    fn const_inline() {
        let mut solver = TestSolver::default();

        let x = solver.new_variable(-1, 1);
        let y = solver.new_variable(-1, -1);
        let constraint_tag = solver.new_constraint_tag();

        let prop_id = solver
            .new_propagator(LinearLessOrEqualPropagatorArgs {
                x: [x, y].into(),
                c: 0,
                constraint_tag,
            })
            .expect("Expected no error to be detected");
        let prop = solver.propagator_store[prop_id]
            .downcast_ref::<LinearLessOrEqualPropagator<crate::variables::DomainId>>()
            .expect("Expected to downcast to LinearLessOrEqualPropagator");
        assert_eq!(prop.c, 1, "RHS has to be equal to negative constant");
        assert_eq!(
            prop.x,
            Box::from(vec![x]),
            "LHS has to have exactly one non-constant term"
        );
    }
}
